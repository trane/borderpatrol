package com.lookout.borderpatrol.sessionx

import java.util.concurrent.TimeUnit
import com.lookout.borderpatrol.crypto.Generator
import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.bijection.Injection
import com.twitter.finagle.http.{Response, Request, Cookie}
import com.twitter.util._

import scala.util.{Success, Failure, Try}

/**
 * This type represents the value of a user's [[com.twitter.finagle.http.Cookie Cookie]]
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
case class SignedId(expires: Time, entropy: Entropy, secret: Secret, tag: Tag, signature: Signature) {
  def toLogIdString: String = Base64StringEncoder.encode(signature.toArray)
}

object SignedId {

  import Generator.EntropyGenerator

  val entropySize: Size = 16
  val lifetime = Duration(1, TimeUnit.DAYS)
  val sessionIdCookieName: String = "border_session"

  private[this] def currentExpiry: Time =
    Time.now + lifetime

  private[sessionx] def expired(t: Time): Boolean =
    t < Time.now || t > currentExpiry

  private[sessionx] def expired(signedId: SignedId): Boolean =
    expired(signedId.expires)

  private[sessionx] def genEntropy: Entropy =
    EntropyGenerator(entropySize)

  private[sessionx] def timeBytes(t: Time): TimeBytes =
    Injection.long2BigEndian(t.inMilliseconds)

  private[sessionx] def payload(t: Time, e: Entropy, i: SecretId, tagId: TagId): Payload =
    timeBytes(t) ++ e ++ i :+ tagId /* Append the IndexSeq and Bytes to form payload */

  private[sessionx] def payload(signedId: SignedId): Payload =
    payload(signedId.expires, signedId.entropy, signedId.secret.id, signedId.tag.id)

  private[sessionx] def signWith(signedId: SignedId, s: Secret): Signature =
    s.sign(payload(signedId))

  private[sessionx] def toIndexedSeq(signedId: SignedId): IndexedSeq[Byte] =
    payload(signedId) ++ signedId.signature

  private[sessionx] def toArray(signedId: SignedId): Array[Byte] =
    toIndexedSeq(signedId).toArray

  /**
   * Generate a new id wrapped in a [[com.twitter.util.Future Future]] since entropy is blocking on the JVM
   *
   * @param store where to fetch the current [[Secret]] to sign this id
   * @return
   */
  def untagged(implicit store: SecretStoreApi): Future[SignedId] =
    (SignedId(currentExpiry, genEntropy, store.current, Untagged)).toFuture
  def authenticated(implicit store: SecretStoreApi): Future[SignedId] =
    (SignedId(currentExpiry, genEntropy, store.current, AuthenticatedTag)).toFuture

  /**
   * Creates [[com.lookout.borderpatrol.sessionx.SignedId SignedId]] instances based on existing values
   */
  def apply(expires: Time, entropy: Entropy, secret: Secret, tag: Tag): SignedId =
    new SignedId(expires, entropy, secret, tag, secret.sign(payload(expires, entropy, secret.id, tag.id)))

  import SignedIdEncoder._

  def as[A](signedId: SignedId)(implicit ev: SignedIdEncoder[A]): A =
    ev.encode(signedId)

  def from[A](a: A)(implicit ev: SignedIdEncoder[A]): Try[SignedId] =
    ev.decode(a)

  def toBase64(signedId: SignedId): String =
    Base64StringEncoder.encode(toArray(signedId))

  def toCookie(signedId: SignedId, cookieName: String): Cookie =
    tap(new Cookie(cookieName, toBase64(signedId))) { cookie =>
      cookie.path = "/"
      cookie.httpOnly = true
      cookie.maxAge = Time.now.until(signedId.expires)
    }

  def toExpiredCookie(cookieName: String): Cookie =
    tap(new Cookie(cookieName, "")) { cookie =>
      cookie.path = "/"
      cookie.httpOnly = true
      cookie.isDiscard = true
      cookie.maxAge = Duration(0, TimeUnit.SECONDS)
    }

  private[this] def fromCookie(cooki: Option[Cookie], cookieName: String)(implicit ev: SecretStoreApi): Try[SignedId] =
    cooki match {
      case Some(cookie) => SignedId.from[Cookie](cookie)
      case None => Failure(SignedIdError(s"no ${cookieName} cookie found"))
    }

  def fromRequest(req: Request, cookieName: String = SignedId.sessionIdCookieName)(implicit ev: SecretStoreApi):
      Try[SignedId] =
    fromCookie(req.cookies.get(cookieName), cookieName)

  def fromResponse(rep: Response, cookieName: String = SignedId.sessionIdCookieName)(implicit ev: SecretStoreApi):
      Try[SignedId] =
    fromCookie(rep.cookies.get(cookieName), cookieName)

  object SignedIdInjections {

    type BytesTuple = (Payload, TimeBytes, Entropy, SecretId, TagId, Signature)

    val timeBytesSize: Size = 8 // long -> bytes
    val signatureSize: Size = 32 // sha256 -> bytes
    val secretIdSize: Size = 2 // secret id byte2
    val tagIdSize: Size = 1 // tag id byte
    val payloadSize: Size = timeBytesSize + SignedId.entropySize + secretIdSize + tagIdSize
    val expectedSize: Size = payloadSize + signatureSize

    def invalid(sig1: Signature, sig2: Signature): Boolean =
      sig1 != sig2

    def validate(t: Time, sig1: Signature, sig2: Signature): Try[Unit] =
      if (SignedId.expired(t)) Failure(new SignedIdError(s"Expired $t"))
      else if (invalid(sig1, sig2)) Failure(new SignedIdError("Signature is invalid"))
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
        case None => Failure(new SignedIdError(s"No secret with id=$bytes"))
      }

    implicit def bytes2Tuple(bytes: IndexedSeq[Byte]): Try[BytesTuple] = bytes match {
      case a if a.size == expectedSize => {
        val (pl, sig) = a.splitAt(payloadSize)
        val (tb, tail1) = pl.splitAt(timeBytesSize)
        val (ent, tail2) = tail1.splitAt(SignedId.entropySize)
        val (secretIdList, tagList) = tail2.splitAt(secretIdSize)
        Success((pl, tb, ent, secretIdList, tagList.head, sig))
      }
      case _ => Failure(new SignedIdError("Not a session string"))
    }

    implicit def seq2SignedId(bytes: IndexedSeq[Byte])(implicit store: SecretStoreApi): Try[SignedId] = for {
      (pyld, tbs, ent, secretId, tagId, sig) <- bytes2Tuple(bytes)
      time <- bytes2Time(tbs)
      secret <- bytes2Secret(secretId)
      _ <- validate(time, sig, secret.sign(pyld))
    } yield new SignedId(time, ent, secret, Tag(tagId), sig)

    implicit def str2arr(s: String): Array[Byte] =
      Base64StringEncoder.decode(s)

    def arr2seq(bytes: Array[Byte]): IndexedSeq[Byte] =
      bytes.toIndexedSeq

    implicit def str2seq(s: String): IndexedSeq[Byte] =
      arr2seq(str2arr(s))

    implicit def str2SignedId(s: String)(implicit store: SecretStoreApi): Try[SignedId] =
      seq2SignedId(str2seq(s))
  }
}
