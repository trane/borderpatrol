package com.lookout.borderpatrol.sessionx

import java.util.concurrent.TimeUnit
import com.lookout.borderpatrol.crypto.Generator
import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.bijection.Injection
import com.twitter.finagle.httpx.{Response, Request, Cookie}
import com.twitter.util._

import scala.util.{Success, Failure, Try}

/**
 * This type represents the value of a user's [[com.twitter.finagle.httpx.Cookie Cookie]]
 * The design of this is to act as a cryptographically verifiable identifier for a user
 *
 * In [[String]] form, is a `Base64` encoded concatenation of the following transformation:
 *  expires: [[com.twitter.util.Time Time]] => `Long` => `Array[Byte]`
 *  entropy: `Array[Byte]`
 *  secret: [[Secret]] => [[SecretId]]
 *  signature: `Array[Byte]`
 *
 * @param expires the time at which this id is expired
 * @param entropy the random bytes unique to this session id
 * @param secret the secret used to sign this session id
 * @param signature the bytes of the signature(expires, entropy, secret.id)
 */
case class SessionId(expires: Time, entropy: Entropy, secret: Secret, signature: Signature)

object SessionId {

  import Generator.EntropyGenerator

  val entropySize: Size = 16
  val lifetime = Duration(1, TimeUnit.DAYS)

  private[this] def currentExpiry: Time =
    Time.now + lifetime

  private[sessionx] def expired(t: Time): Boolean =
    t < Time.now || t > currentExpiry

  private[sessionx] def expired(id: SessionId): Boolean =
    expired(id.expires)

  private[sessionx] def genEntropy: Entropy =
    EntropyGenerator(entropySize)

  private[sessionx] def timeBytes(t: Time): TimeBytes =
    Injection.long2BigEndian(t.inMilliseconds)

  private[sessionx] def payload(t: Time, e: Entropy, i: SecretId): Payload =
    timeBytes(t) ++ e :+ i

  private[sessionx] def payload(id: SessionId): Payload =
    payload(id.expires, id.entropy, id.secret.id)

  private[sessionx] def signWith(id: SessionId, s: Secret): Signature =
    s.sign(payload(id))

  private[sessionx] def toIndexedSeq(id: SessionId): IndexedSeq[Byte] =
    payload(id) ++ id.signature

  private[sessionx] def toArray(id: SessionId): Array[Byte] =
    toIndexedSeq(id).toArray

  /**
   * Generate a new id wrapped in a [[com.twitter.util.Future Future]] since entropy is blocking on the JVM
   *
   * @param store where to fetch the current [[Secret]] to sign this id
   * @return
   */
  def next(implicit store: SecretStoreApi): Future[SessionId] =
    (SessionId(currentExpiry, genEntropy, store.current)).toFuture

  /**
   * Creates [[com.lookout.borderpatrol.sessionx.SessionId SessionId]] instances based on existing values
   */
  def apply(expires: Time, entropy: Entropy, secret: Secret): SessionId =
    new SessionId(expires, entropy, secret, secret.sign(payload(expires, entropy, secret.id)))

  import SessionIdEncoder._

  def as[A](id: SessionId)(implicit ev: SessionIdEncoder[A]): A =
    ev.encode(id)

  def from[A](a: A)(implicit ev: SessionIdEncoder[A]): Try[SessionId] =
    ev.decode(a)

  def toBase64(id: SessionId): String =
    Base64StringEncoder.encode(toArray(id))

  def toCookie(id: SessionId): Cookie =
    tap(new Cookie("border_session", toBase64(id))) { cookie =>
      cookie.path = "/"
      cookie.httpOnly = true
      cookie.maxAge = Time.now.until(id.expires)
    }

  def fromCookie(cooki: Option[Cookie])(implicit ev: SecretStoreApi): Try[SessionId] =
    cooki match {
      case Some(cookie) => SessionId.from[Cookie](cookie)
      case None => Failure(SessionIdError("no border_session cookie"))
    }

  def fromRequest(req: Request)(implicit ev: SecretStoreApi): Try[SessionId] =
    fromCookie(req.cookies.get("border_session"))

  def fromResponse(rep: Response)(implicit ev: SecretStoreApi): Try[SessionId] =
    fromCookie(rep.cookies.get("border_session"))

  object SessionIdInjections {

    type BytesTuple = (Payload, TimeBytes, Entropy, SecretId, Signature)

    val timeBytesSize: Size = 8 // long -> bytes
    val signatureSize: Size = 32 // sha256 -> bytes
    val secretIdSize: Size = 1 // secret id byte
    val payloadSize: Size = timeBytesSize + SessionId.entropySize + secretIdSize
    val expectedSize: Size = payloadSize + signatureSize

    def invalid(sig1: Signature, sig2: Signature): Boolean =
      sig1 != sig2

    def validate(t: Time, sig1: Signature, sig2: Signature): Try[Unit] =
      if (SessionId.expired(t)) Failure(new SessionIdError(s"Expired $t"))
      else if (invalid(sig1, sig2)) Failure(new SessionIdError("Signature is invalid"))
      else Success(())

    def long2Time(l: Long): Time =
      Time.fromMilliseconds(l)

    def bytes2Long(bytes: IndexedSeq[Byte]): Try[Long] =
      Injection.long2BigEndian.invert(bytes.toArray)

    implicit def bytes2Time(bytes: IndexedSeq[Byte]): Try[Time] = for {
      l <- bytes2Long(bytes)
    } yield long2Time(l)

    implicit def byte2Secret(byte: Byte)(implicit store: SecretStoreApi): Try[Secret] =
      store.find(_.id == byte) match {
        case Some(s) => Success(s)
        case None => Failure(new SessionIdError(s"No secret with id=$byte"))
      }

    implicit def bytes2Tuple(bytes: IndexedSeq[Byte]): Try[BytesTuple] = bytes match {
      case a if a.size == expectedSize => {
        val (pl, sig) = a.splitAt(payloadSize)
        val (tb, tail) = pl.splitAt(timeBytesSize)
        val (ent, idList) = tail.splitAt(SessionId.entropySize)
        Success((pl, tb, ent, idList.head, sig))
      }
      case _ => Failure(new SessionIdError("Not a session string"))
    }

    implicit def seq2SessionId(bytes: IndexedSeq[Byte])(implicit store: SecretStoreApi): Try[SessionId] = for {
      (pyld, tbs, ent, id, sig) <- bytes2Tuple(bytes)
      time <- bytes2Time(tbs)
      secret <- byte2Secret(id)
      _ <- validate(time, sig, secret.sign(pyld))
    } yield new SessionId(time, ent, secret, sig)

    implicit def str2arr(s: String): Array[Byte] =
      Base64StringEncoder.decode(s)

    def arr2seq(bytes: Array[Byte]): IndexedSeq[Byte] =
      bytes.toIndexedSeq

    implicit def str2seq(s: String): IndexedSeq[Byte] =
      arr2seq(str2arr(s))

    implicit def str2SessionId(s: String)(implicit store: SecretStoreApi): Try[SessionId] =
      seq2SessionId(str2seq(s))

  }
}
