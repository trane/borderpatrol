package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit

import com.twitter.bijection._
import com.twitter.util.{Duration, Time}

import scala.util.{Failure, Success, Try}

object SessionIdTypes {
  type Seconds = Long
  type Size = Int
  type TimeBytes = List[Byte]
  type Entropy = List[Byte]
  type SecretId = Byte
  type Signature = List[Byte]
  type Payload = List[Byte]
}

trait SessionIdExpiryComp extends Expiry {
  val lifetime = Duration(1, TimeUnit.DAYS)
}

trait SecureSessionIdComponent {
  this: SecretStoreComponent with SessionIdExpiryComp =>
    implicit val secretStore: SecretStoreApi
}

sealed trait SessionId extends SessionIdExpiryComp {
  import com.lookout.borderpatrol.session.SessionIdTypes._

  val expires: Time
  val entropy: Entropy
  val secretId: SecretId
  val signature: Signature

  lazy val payload: Payload = timeBytes ++ entropy :+ secretId
  lazy val toSeq: Seq[Byte] = payload ++ signature
  lazy val toBytes: Array[Byte] = toSeq.toArray

  def timeBytes: TimeBytes = Injection.long2BigEndian(expires.inLongSeconds).toList
  def expired: Boolean = expires < Time.now || expires > currentExpiry
}

class SessionIdGenerator extends SessionIdExpiryComp {
  import com.lookout.borderpatrol.session.Constants.SessionId.entropySize
  import com.lookout.borderpatrol.session.SessionIdTypes._

  def next(implicit store: SecretStoreApi): SessionId =
    Id(currentExpiry, Generator(entropySize).toList)(store.current)

  case class Id(expires: Time, entropy: Entropy)(secret: Secret) extends SessionId {
    val secretId: SecretId = secret.id
    val signature: Signature = secret.sign(payload).toList
  }
}

case class SessionIdMarshaller(store: SecretStoreApi) {
  def encode(s: SessionId): String = injector.idAndSecret2String((s, null))
  def decode(s: String): Try[SessionId] = injector.idAndSecret2String.invert(s).map(_._1)
  def decodeWithSecret(s: String): Try[(SessionId, Secret)] = injector.idAndSecret2String.invert(s)

  object injector extends SessionIdExpiryComp {
    import com.lookout.borderpatrol.session.Constants.SessionId.entropySize
    import com.lookout.borderpatrol.session.SessionIdTypes._

    type BytesTuple = (Payload, TimeBytes, Entropy, SecretId, Signature)

    val timeBytesSize: Size = 8 // long -> bytes
    val signatureSize: Size = 32 // sha256 -> bytes
    val secretIdSize: Size = 1 // secret id byte
    val payloadSize: Size = timeBytesSize + entropySize + secretIdSize
    val expectedSize: Size = payloadSize + signatureSize

    def long2Time(l: Long): Try[Time] = {
      val t = Time.fromMilliseconds(l * 1000L)
      if (t > Time.now && t <= currentExpiry) Success(t)
      else Failure(new Exception("Time has expired"))
    }

    def validOrThrow(s: Signer, p: Payload, sig: Signature): Boolean =
      if (s.sign(p) == sig) true
      else throw new Exception("Invalid signature")

    def parseBytes(bytes: List[Byte]): Try[BytesTuple] = bytes match {
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
      } yield (Id(time, ent, id, sig), sec)
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
          for (t <- parseBytes(bytes.toList); s <- sessionId(t)) yield s
      }

    implicit lazy val idAndSecret2String = Injection.connect[(SessionId, Secret), Array[Byte], Base64String, String]
    implicit lazy val idAndSecret2Id = Injection.connect[(SessionId, Secret), SessionId]

    case class Id(expires: Time, entropy: Entropy, secretId: SecretId, signature: Signature) extends SessionId
  }
}
