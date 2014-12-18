package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit

import com.twitter.bijection._
import com.twitter.util.{Duration, Time}

import scala.util.{Failure, Success, Try}

object SessionIdTypes {
  type Seconds = Long
  type Size = Int
  type TimeBytes = Array[Byte]
  type Entropy = Array[Byte]
  type SecretId = Byte
  type Signature = Array[Byte]
  type Payload = Array[Byte]
}

trait SessionIdExpiryComp extends Expiry {
  val lifetime = Duration(1, TimeUnit.DAYS)
}

trait SecureSessionIdComponent {
  this: SecretStoreComponent with SessionIdExpiryComp =>
    implicit val secretStore: SecretStoreApi
}

sealed trait SessionId extends SessionIdExpiryComp {
  import SessionIdTypes._
  val expires: Time
  val entropy: Entropy
  val secretId: SecretId
  val signature: Signature
  lazy val toBytes: Array[Byte] = timeBytes ++ entropy ++ (secretId +: signature)

  def timeBytes: TimeBytes = Injection.long2BigEndian(expires.inLongSeconds)
  def expired: Boolean = expires < Time.now || expires > currentExpiry
}

class SessionIdGenerator extends SessionIdExpiryComp {
  import SessionIdTypes._
  import Constants.SessionId.entropySize

  type GenPayload = (Time, Entropy) => Payload
  type PayloadSigner = Payload => (Signature, SecretId)
  type GenSignature = Secret => PayloadSigner

  def timeBytes(t: Time): TimeBytes = Injection.long2BigEndian(t.inLongSeconds)

  def genPayload: GenPayload = (t, ent) => timeBytes(t) ++ ent

  def genSignature: GenSignature = (s) => (bytes) => (s.sign(bytes :+ s.id), s.id)

  def next(implicit store: SecretStoreApi): SessionId =
    NextSessionId(currentExpiry, Generator(entropySize))(genPayload, genSignature(store.current))

  case class NextSessionId(expires: Time, entropy: Entropy)(pGen: GenPayload, signer: PayloadSigner) extends SessionId {
    val (signature, secretId) = signer(pGen(expires, entropy))
  }
}

sealed trait Marshallable {
  def encode(s: SessionId): String
  def decode(s: String): Try[SessionId]
}

case class SessionIdMarshaller(store: SecretStoreApi) extends Marshallable {

  def encode(s: SessionId): String = injector.sessionId2String(s)
  def decode(s: String): Try[SessionId] = injector.sessionId2String.invert(s)

  object injector extends SessionIdExpiryComp {
    import SessionIdTypes._
    import Constants.SessionId.entropySize

    type BytesTuple = (TimeBytes, Entropy, SecretId, Signature)

    val timeBytesSize: Size = 8 // long -> bytes
    val signatureSize: Size = 32 // sha256 -> bytes
    val secretIdSize: Size = 1 // secret id byte
    val expectedSize: Size = timeBytesSize + entropySize + secretIdSize + signatureSize

    def long2Time(l: Long): Try[Time] = {
      val t = Time.fromMilliseconds(l * 1000L)
      if (t > Time.now && t <= currentExpiry) Success(t)
      else Failure(new Exception("Time has expired"))
    }

    def validOrThrow(s: Signer, p: Payload, sig: Signature): Boolean =
      if (s.sign(p).sameElements(sig)) true
      else throw new Exception("Invalid signature")


    def parseBytes(bytes: Array[Byte]): Try[BytesTuple] = bytes match {
      case a if a.size == expectedSize => {
        val (tb, tail) = a.splitAt(timeBytesSize)
        val (ent, last) = tail.splitAt(entropySize)
        val (id, sig) = last.splitAt(secretIdSize)
        Success(tb, ent, id.head, sig)
      }
      case _ => Failure(new Exception("Not a session string"))
    }

    def sessionId(tuple: BytesTuple): Try[SessionId] = {
      val (tb, ent, id, sig) = tuple
      for {
        tLong <- Injection.long2BigEndian.invert(tb)
        time <- long2Time(tLong)
        s <- store.find((s) => s.id == id && validOrThrow(s, tb ++ ent :+ id, sig))
      } yield ExistingSession(time, ent, id, sig)
    }

    implicit lazy val sessionId2Bytes: Injection[SessionId, Array[Byte]] =
      new AbstractInjection[SessionId, Array[Byte]] {
        def apply(sid: SessionId): Array[Byte] = sid.toBytes

        override def invert(bytes: Array[Byte]): Try[SessionId] =
          for (t <- parseBytes(bytes); s <- sessionId(t)) yield s
      }

    implicit lazy val sessionId2String = Injection.connect[SessionId, Array[Byte], Base64String, String]

    case class ExistingSession(expires: Time, entropy: Entropy, secretId: SecretId, signature: Signature) extends SessionId
  }
}
