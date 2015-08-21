package com.lookout.borderpatrol.sessionx

import argonaut.Json
import com.lookout.borderpatrol.sessionx.SessionId.SessionIdInjections
import com.twitter.finagle.httpx.Cookie
import com.twitter.io.Buf
import com.twitter.finagle.httpx
import scala.util.{Success, Failure, Try}

/**
 * Typeclass for Encoding data within sessions
 */
trait Encoder[A, B] {
  def encode(a: A): B
  def decode(b: B): Try[A]
}
trait SessionDataEncoder[A] extends Encoder[A, Buf]
trait SessionIdEncoder[A] extends Encoder[SessionId, A]
trait SecretEncoder[A] extends Encoder[Secret, A]

/**
 * Instances of SessionDataEncoder for A => [[com.twitter.io.Buf]]
 */
object SessionDataEncoder {
  /**
   * Helper method for creating new [[com.lookout.borderpatrol.sessionx.SessionDataEncoder]] instances
   */
  def apply[A](f: A => Buf, g: Buf => A): SessionDataEncoder[A] = new SessionDataEncoder[A] {
    def encode(data: A): Buf = f(data)
    def decode(buf: Buf): Try[A] = Try { g(buf) } match {
      case Failure(e) => Failure(SessionDataError(e))
      case s => s
    }
  }

  /**
   * A [[com.lookout.borderpatrol.sessionx.SessionDataEncoder SessionDataEncoder]] instance for
   * [[com.twitter.finagle.httpx.Request httpx.Request]]
   */
  implicit val encodeRequest: SessionDataEncoder[httpx.Request] = SessionDataEncoder(
    data => Buf.ByteArray.Owned(data.encodeBytes()),
    buf => httpx.Request.decodeBytes(Buf.ByteArray.Owned.extract(buf))
  )

  /**
   * A [[com.lookout.borderpatrol.sessionx.SessionDataEncoder SessionDataEncoder]] instance for [[java.lang.String]]
   */
  implicit val encodeString: SessionDataEncoder[String] = SessionDataEncoder(
    data => Buf.Utf8.apply(data),
    buf => Buf.Utf8.unapply(buf) getOrElse (throw new SessionError("Buf conversion to String failed"))
  )

  /**
   * A [[com.lookout.borderpatrol.sessionx.SessionDataEncoder SessionDataEncoder]] instance for Array[Byte]
   */
  implicit val encodeByteArray: SessionDataEncoder[Array[Byte]] = SessionDataEncoder(
    data => Buf.ByteArray.Owned(data),
    buf => Buf.ByteArray.Owned.extract(buf)
  )

  /**
   * A [[com.lookout.borderpatrol.sessionx.SessionDataEncoder SessionDataEncoder]] instance for Int
   */
  implicit val encodeInt: SessionDataEncoder[Int] = SessionDataEncoder(
    data => Buf.U32BE(data),
    buf => Buf.U32BE.unapply(buf).map(t => t._1) getOrElse (throw new SessionError("Buf conversion to Int failed"))


  )

}

/**
 * Instances of SessionIdEncoder for [[com.lookout.borderpatrol.sessionx.SessionId SessionId]] => A
 */
object SessionIdEncoder {
  /**
   * Helper method for creating new [[com.lookout.borderpatrol.sessionx.SessionIdEncoder]] instances
   */
  def apply[A](f: SessionId => A, g: A => Try[SessionId]): SessionIdEncoder[A] =
    new SessionIdEncoder[A] {
      def encode(id: SessionId): A = f(id)
      def decode(a: A): Try[SessionId] = g(a)
    }

  /**
   * A [[com.lookout.borderpatrol.sessionx.SessionIdEncoder SessionIdEncoder]] instance for
   * [[java.lang.String String]]
   */
  implicit def encodeString(implicit secretStoreApi: SecretStoreApi): SessionIdEncoder[String] = SessionIdEncoder(
    id => SessionId.toBase64(id),
    str => SessionIdInjections.str2SessionId(str)
  )

  /**
   * A [[com.lookout.borderpatrol.sessionx.SessionIdEncoder SessionIdEncoder]] instance for
   * [[com.twitter.finagle.httpx.Cookie Cookie]]
   */
  implicit def encodeCookie(implicit secretStoreApi: SecretStoreApi): SessionIdEncoder[Cookie] = SessionIdEncoder(
    id => new Cookie("border_session", SessionId.toBase64(id)),
    cookie => SessionIdInjections.str2SessionId(cookie.value)
  )

}

/**
 * Instances of SecretEncoder for [[com.lookout.borderpatrol.sessionx.Secret Secret]] => A
 */
object SecretEncoder {
  /**
   * Helper method for creating new [[com.lookout.borderpatrol.sessionx.SecretEncoder SecretEncoder]] instances
   */
  def apply[A](f: Secret => A, g: A => Try[Secret]): SecretEncoder[A] =
    new SecretEncoder[A] {
      def encode(secret: Secret): A = f(secret)
      def decode(a: A): Try[Secret] = g(a)
    }

  /**
   * A [[com.lookout.borderpatrol.sessionx.SecretEncoder SecretEncoder]] instance for [[argonaut.Json Json]]
   */
  implicit object EncodeJson extends SecretEncoder[Json] {
    import argonaut._, Argonaut._

    implicit val SecretCodecJson: argonaut.CodecJson[Secret] =
      casecodec3(Secret.apply, Secret.unapply)("expiry", "id", "entropy")

    def encode(secret: Secret): Json =
      secret.asJson

    def decode(json: Json): Try[Secret] =
      json.jdecode[Secret].toDisjunction.fold[Try[Secret]](
        e => Failure(SecretDecodeError(e._1)),
        s => Success(s)
      )
  }


}
