package com.lookout.borderpatrol

import java.util.concurrent.TimeUnit

import com.twitter.bijection._
import com.twitter.util.{Duration, Time}

import scala.util.{Success, Failure, Try}

trait SecureSessionId {
  this: ExternalSecretStore with Expiry =>
    def secretStore: SecretStore = SecretStore(Secrets(Current(SessionExpiry.currentExpiry), None))
    val lifetime: Duration = Duration(1, TimeUnit.DAYS)
}

object SecureSessionId extends SecureSessionId with ExternalSecretStore with Expiry {
  type Seconds = Long
  type Size = Int
  type TimeBytes = Array[Byte]
  type Entropy = Array[Byte]
  type SecretId = Array[Byte]
  type Signature = Array[Byte]
  type Payload = Array[Byte]

  sealed trait SessionId {
    val expires: TimeBytes
    val entropy: Entropy
    val secretId: SecretId
    val signature: Signature
    lazy val repr: String = SessionIdSerializer.encode(this)
    lazy val toBytes: Array[Byte] = expires ++ entropy ++ secretId ++ signature

    def expired: Boolean = {
      val t: Seconds = Injection.long2BigEndian.invert(expires).getOrElse(0)
      t < Time.now.inLongSeconds && t > SessionExpiry.currentExpiry.inLongSeconds
    }
    override def toString: String = repr
  }

  val entropySize: Size = 16 // bytes

  def apply: SessionId = SessionIdGenerator.next
  def apply(s: String): Try[SessionId] = SessionIdSerializer.decode(s)
}

object SessionIdSerializer {
  import SecureSessionId._

  def encode(s: SessionId): String = Bijector.sessionId2String(s)
  def decode(s: String): Try[SessionId] = Bijector.sessionId2String.invert(s)

  object Bijector {
    type BytesTuple = (TimeBytes, Entropy, SecretId, Signature)

    val timeBytesSize: Size = 8 // long -> bytes
    val signatureSize: Size = 32 // sha256 -> bytes
    val secretIdSize: Size = 1 // secret id
    val expectedSize: Size = timeBytesSize + entropySize + secretIdSize + signatureSize

    def notExpiredOrThrow(t: Seconds): Boolean =
      if (t > Time.now.inLongSeconds && t <= SessionExpiry.currentExpiry.inLongSeconds) true
      else throw new Exception("Time has expired")

    def validOrThrow(s: Signer, p: Payload, sig: Signature): Boolean =
      if (s.sign(p).sameElements(sig)) true
      else throw new Exception("Invalid signature")


    def parseBytes(bytes: Array[Byte]): Try[BytesTuple] = bytes match {
      case a if a.size == expectedSize => {
        val (tb, tail) = a.splitAt(timeBytesSize)
        val (ent, last) = tail.splitAt(entropySize)
        val (id, sig) = last.splitAt(secretIdSize)
        Success(tb, ent, id, sig)
      }
      case _ => Failure(new Exception("Not a session string"))
    }

    def sessionId(tuple: BytesTuple): Try[SessionId] = {
      val (tb, ent, id, sig) = tuple
      (for {
        t <- Injection.long2BigEndian.invert(tb)
        if notExpiredOrThrow(t)
        s <- secretStore.find((s) => s.id.sameElements(id) && validOrThrow(s, tb ++ ent ++ id, sig))
      } yield IdFromTuple(tuple))
    }

    implicit lazy val sessionId2Bytes: Injection[SessionId, Array[Byte]] =
      new AbstractInjection[SessionId, Array[Byte]] {
        def apply(sid: SessionId): Array[Byte] = sid.toBytes

        override def invert(bytes: Array[Byte]): Try[SessionId] =
          for (t <- parseBytes(bytes); s <- sessionId(t)) yield s
      }

    implicit lazy val sessionId2String = Injection.connect[SessionId, Array[Byte], Base64String, String]

    case class IdFromTuple(tuple: BytesTuple) extends SessionId {
      val (expires, entropy, secretId, signature) = tuple
    }
  }

}

object SessionIdGenerator {
  import SecureSessionId._

  type GenTimeBytes = PartialFunction[Seconds, Option[TimeBytes]]
  type GenEntropy = PartialFunction[Size, Option[Entropy]]
  type GenPayload = (TimeBytes, Entropy) => Payload
  type PayloadSigner = Payload => (Signature, SecretId)
  type GenSignature = Secret => PayloadSigner

  def genTimeBytes: GenTimeBytes = {
    case s if s > Time.now.inLongSeconds => Some(Injection.long2BigEndian(s))
    case _ => None
  }

  def genEntropy: GenEntropy = {
    case SecureSessionId.entropySize => Some(Generator(entropySize))
    case _ => None
  }

  def genPayload: GenPayload = (tb, ent) => tb ++ ent

  def genSignature: GenSignature = (s) => (bytes) => (s.sign(bytes ++ s.id), s.id)

  def next: SessionId = (for {
    t <- genTimeBytes(SessionExpiry.currentExpiry.inLongSeconds)
    e <- genEntropy(entropySize)
  } yield Id(t, e)(genPayload, genSignature(secretStore.current))).get

  case class Id(expires: TimeBytes, entropy: Entropy)(pGen: GenPayload, signer: PayloadSigner) extends SessionId {
    val (signature, secretId) = signer(pGen(expires, entropy))
  }
}
