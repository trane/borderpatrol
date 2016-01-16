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
 *  tag: => [[Tag]]
 *  signature: `Array[Byte]`
 *
 * @param expires the time at which this id is expired
 * @param entropy the random bytes unique to this session id
 * @param secret the secret used to sign this session id
 * @param signature the bytes of the signature(expires, entropy, secret.id)
 */
case class SessionId(expires: Time, entropy: Entropy, secret: Secret, tag: Tag, signature: Signature) {
  def toLogIdString: String = Base64StringEncoder.encode(signature.toArray)
}

object SessionId {

  import Generator.EntropyGenerator

  val entropySize: Size = 16
  val lifetime = Duration(1, TimeUnit.DAYS)

  private[this] def currentExpiry: Time =
    Time.now + lifetime

  private[sessionx] def expired(t: Time): Boolean =
    t < Time.now || t > currentExpiry

  private[sessionx] def expired(sessionId: SessionId): Boolean =
    expired(sessionId.expires)

  private[sessionx] def genEntropy: Entropy =
    EntropyGenerator(entropySize)

  private[sessionx] def timeBytes(t: Time): TimeBytes =
    Injection.long2BigEndian(t.inMilliseconds)

  private[sessionx] def payload(t: Time, e: Entropy, i: SecretId, tagId: TagId): Payload =
    timeBytes(t) ++ e ++ i :+ tagId /* Append the IndexSeq and Bytes to form payload */

  private[sessionx] def payload(sessionId: SessionId): Payload =
    payload(sessionId.expires, sessionId.entropy, sessionId.secret.id, sessionId.tag.id)

  private[sessionx] def signWith(sessionId: SessionId, s: Secret): Signature =
    s.sign(payload(sessionId))

  private[sessionx] def toIndexedSeq(sessionId: SessionId): IndexedSeq[Byte] =
    payload(sessionId) ++ sessionId.signature

  private[sessionx] def toArray(sessionId: SessionId): Array[Byte] =
    toIndexedSeq(sessionId).toArray

  /**
   * Generate a new id wrapped in a [[com.twitter.util.Future Future]] since entropy is blocking on the JVM
   *
   * @param store where to fetch the current [[Secret]] to sign this id
   * @return
   */
  def untagged(implicit store: SecretStoreApi): Future[SessionId] =
    (SessionId(currentExpiry, genEntropy, store.current, Untagged)).toFuture
  def authenticated(implicit store: SecretStoreApi): Future[SessionId] =
    (SessionId(currentExpiry, genEntropy, store.current, AuthenticatedTag)).toFuture

  /**
   * Creates [[com.lookout.borderpatrol.sessionx.SessionId SessionId]] instances based on existing values
   */
  def apply(expires: Time, entropy: Entropy, secret: Secret, tag: Tag): SessionId =
    new SessionId(expires, entropy, secret, tag, secret.sign(payload(expires, entropy, secret.id, tag.id)))

  import SessionIdEncoder._

  def as[A](sessionId: SessionId)(implicit ev: SessionIdEncoder[A]): A =
    ev.encode(sessionId)

  def from[A](a: A)(implicit ev: SessionIdEncoder[A]): Try[SessionId] =
    ev.decode(a)

  def toBase64(sessionId: SessionId): String =
    Base64StringEncoder.encode(toArray(sessionId))

  def toCookie(sessionId: SessionId, expired: Boolean = false): Cookie =
    tap(new Cookie("border_session", toBase64(sessionId))) { cookie =>
      cookie.path = "/"
      cookie.httpOnly = true
      cookie.isDiscard = expired
      cookie.maxAge = if (expired) Duration(0, TimeUnit.SECONDS) else Time.now.until(sessionId.expires)
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

    type BytesTuple = (Payload, TimeBytes, Entropy, SecretId, TagId, Signature)

    val timeBytesSize: Size = 8 // long -> bytes
    val signatureSize: Size = 32 // sha256 -> bytes
    val secretIdSize: Size = 2 // secret id byte2
    val tagIdSize: Size = 1 // tag id byte
    val payloadSize: Size = timeBytesSize + SessionId.entropySize + secretIdSize + tagIdSize
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

    implicit def bytes2Secret(bytes: IndexedSeq[Byte])(implicit store: SecretStoreApi): Try[Secret] =
      store.find(_.id == bytes) match {
        case Some(s) => Success(s)
        case None => Failure(new SessionIdError(s"No secret with id=$bytes"))
      }

    implicit def bytes2Tuple(bytes: IndexedSeq[Byte]): Try[BytesTuple] = bytes match {
      case a if a.size == expectedSize => {
        val (pl, sig) = a.splitAt(payloadSize)
        val (tb, tail1) = pl.splitAt(timeBytesSize)
        val (ent, tail2) = tail1.splitAt(SessionId.entropySize)
        val (secretIdList, tagList) = tail2.splitAt(secretIdSize)
        Success((pl, tb, ent, secretIdList, tagList.head, sig))
      }
      case _ => Failure(new SessionIdError("Not a session string"))
    }

    implicit def seq2SessionId(bytes: IndexedSeq[Byte])(implicit store: SecretStoreApi): Try[SessionId] = for {
      (pyld, tbs, ent, secretId, tagId, sig) <- bytes2Tuple(bytes)
      time <- bytes2Time(tbs)
      secret <- bytes2Secret(secretId)
      _ <- validate(time, sig, secret.sign(pyld))
    } yield new SessionId(time, ent, secret, Tag(tagId), sig)

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
