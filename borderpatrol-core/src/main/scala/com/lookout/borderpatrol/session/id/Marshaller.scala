package com.lookout.borderpatrol.session.id

import com.lookout.borderpatrol.session.id.Types._
import com.lookout.borderpatrol.session.secret.SecretStoreApi
import com.lookout.borderpatrol.session.{SessionId, Secret, Signer}
import com.twitter.bijection.{AbstractInjection, Base64String, Injection}
import com.twitter.util.Time

import scala.util.{Failure, Success, Try}

case class Marshaller(store: SecretStoreApi) {
  def encode(s: SessionId): String = injector.idAndSecret2String((s, null))
  def decode(s: String): Try[SessionId] = injector.idAndSecret2String.invert(s).map(_._1)
  def decodeWithSecret(s: String): Try[(SessionId, Secret)] = injector.idAndSecret2String.invert(s)

  object injector extends ExpiryComponent {
    import com.lookout.borderpatrol.session.Constants.SessionId.entropySize

    type BytesTuple = (Payload, TimeBytes, Entropy, SecretId, Signature)

    val timeBytesSize: Size = 8 // long -> bytes
    val signatureSize: Size = 32 // sha256 -> bytes
    val secretIdSize: Size = 1 // secret id byte
    val payloadSize: Size = timeBytesSize + entropySize + secretIdSize
    val expectedSize: Size = payloadSize + signatureSize

    def long2Time(l: Long): Try[Time] = {
      val t = Time.fromMilliseconds(l)
      if (t > Time.now && t <= currentExpiry) Success(t)
      else Failure(new Exception("Time has expired"))
    }

    def validOrThrow(s: Signer, p: Payload, sig: Signature): Boolean =
      if (s.sign(p) == sig) true
      else throw new Exception("Invalid signature")

    def parseBytes(bytes: Vector[Byte]): Try[BytesTuple] = bytes match {
      case a if a.size == expectedSize => {
        val (pl, sig) = a.splitAt(payloadSize)
        val (tb, tail) = pl.splitAt(timeBytesSize)
        val (ent, idList) = tail.splitAt(entropySize)
        Success(pl, tb, ent, idList.head, sig)
      }
      case _ => Failure(new Exception("Not a session string"))
    }

    def sessionId(tuple: BytesTuple): Try[(SessionId, Secret)] = {
      val (pl, tb, ent, id, sig) = tuple
      for {
        tLong <- Injection.long2BigEndian.invert(tb.toArray)
        time <- long2Time(tLong)
        sec <- store.find((s) => s.id == id && validOrThrow(s, pl, sig))
      } yield (SessionId(time, ent, id, sig), sec)
    }

    implicit lazy val secretAndSessionId2SessionId: Injection[(SessionId, Secret), SessionId] =
      new AbstractInjection[(SessionId, Secret), SessionId] {
        def apply(t: (SessionId, Secret)): SessionId = t._1

        override def invert(sid: SessionId): Try[(SessionId, Secret)] =
          for (s <- store.find((s) => s.id == sid.secretId && validOrThrow(s, sid.payload, sid.signature))) yield (sid, s)
      }

    implicit lazy val sessionIdAndSecret2Bytes: Injection[(SessionId, Secret), Array[Byte]] =
      new AbstractInjection[(SessionId, Secret), Array[Byte]] {

        def apply(t: (SessionId, Secret)): Array[Byte] = t._1.toBytes.toArray

        override def invert(bytes: Array[Byte]): Try[(SessionId, Secret)] =
          for (t <- parseBytes(bytes.toVector); s <- sessionId(t)) yield s
      }

    implicit lazy val idAndSecret2String = Injection.connect[(SessionId, Secret), Array[Byte], Base64String, String]
    implicit lazy val idAndSecret2Id = Injection.connect[(SessionId, Secret), SessionId]

  }
}
