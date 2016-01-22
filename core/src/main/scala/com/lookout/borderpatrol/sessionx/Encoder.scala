package com.lookout.borderpatrol.sessionx

import argonaut.Json
import com.lookout.borderpatrol.crypto.{Decryptable, Encryptable}
import com.lookout.borderpatrol.sessionx.SignedId.SignedIdInjections
import com.twitter.finagle.http.Cookie
import com.twitter.io.Buf
import com.twitter.finagle.http
import scala.util.{Success, Failure, Try}
import argonaut._, Argonaut._

/**
 * Typeclasses for Encoding data within sessions
 */
trait Encoder[A, B] {
  def encode(a: A): B
  def decode(b: B): Try[A]
}
trait SessionDataEncoder[A] extends Encoder[A, Buf]
trait SignedIdEncoder[A] extends Encoder[SignedId, A]
trait SecretEncoder[A] extends Encoder[Secret, A]
trait SecretsEncoder[A] extends Encoder[Secrets,A]


/**
 * This type uses the [[com.lookout.borderpatrol.sessionx.SessionDataEncoder SessionDataEncoder]] to encode
 * generic types to [[com.twitter.io.Buf]], then the [[Encryptable]] and [[Decryptable]] types will unwrap the `Buf`
 * encrypt/decrypt it, and then rewrap it.
 */
trait EncryptedDataEncoder[A] {
  val encrypter = Encryptable.encryptableBuf
  val decrypter = Decryptable.decryptableBuf
  implicit val encoder: SessionDataEncoder[A]

  implicit val toArr: Buf => Array[Byte] = buf => Buf.ByteArray.Owned.extract(buf)
  implicit val fromArr: Array[Byte] => Buf = arr => Buf.ByteArray.Owned(arr)

  /**
   * Wrap [[com.lookout.borderpatrol.sessionx.Encoder.encode]] with encryption
   */
  def encrypted(session: Session[A]): Buf =
    fromArr(
      encrypter.encrypt(
        session.map(data => encoder.encode(data))
      )
    )

  /**
   * Wrap [[com.lookout.borderpatrol.sessionx.Encoder.encode]] with decryption
   */
  def decrypted(id: SignedId, buf: Buf): Try[Session[A]] =
    for {
      s <- decrypter.decrypt(id, toArr(buf))
      d <- encoder.decode(s.data)
    } yield Session(id, d)

    //decrypter.decrypt(id, fromArr(bytes)).flatMap(s => encoder.decode(s.data)).map(Session(id, _))
}

/**
 * Instances of EncryptedDataEncoder only need to contain a SessionDataEncoder
 * TODO: remove this whole concept of an EncryptedDataEncoder type class, it's worthless.
 * We can treat [[com.lookout.borderpatrol.sessionx.Session Session]] as a `Functor` and apply the encryption/decryption
 * via mixins to the base [[com.lookout.borderpatrol.sessionx.SessionStore SessionStore]]
 */
object EncryptedDataEncoder {
  implicit object EncryptedStringEncoder extends EncryptedDataEncoder[String] {
    implicit val encoder: SessionDataEncoder[String] = SessionDataEncoder.encodeString
  }
  implicit object EncryptedIntEncoder extends EncryptedDataEncoder[Int] {
    implicit val encoder: SessionDataEncoder[Int] = SessionDataEncoder.encodeInt
  }
  implicit object EncryptedReqEncoder extends EncryptedDataEncoder[http.Request] {
    implicit val encoder: SessionDataEncoder[http.Request] = SessionDataEncoder.encodeRequest
  }
  implicit object EncryptedBytesEncoder extends EncryptedDataEncoder[Array[Byte]] {
    implicit val encoder: SessionDataEncoder[Array[Byte]] = SessionDataEncoder.encodeByteArray
  }
}

/**
 * Create instances of Encoder
 */
object Encoder {
  def apply[A, B](fa: A => B, fb: B => A): Encoder[A, B] = new Encoder[A, B] {
    def encode(a: A): B = fa(a)
    def decode(b: B): Try[A] = Try { fb(b) }
  }
}

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
   * [[com.twitter.finagle.http.Request http.Request]]
   */
  implicit val encodeRequest: SessionDataEncoder[http.Request] = SessionDataEncoder(
    data => Buf.ByteArray.Owned(data.encodeBytes()),
    buf => http.Request.decodeBytes(Buf.ByteArray.Owned.extract(buf))
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
 * Instances of SignedIdEncoder for [[com.lookout.borderpatrol.sessionx.SignedId SignedId]] => A
 */
object SignedIdEncoder {
  /**
   * Helper method for creating new [[com.lookout.borderpatrol.sessionx.SignedIdEncoder]] instances
   */
  def apply[A](f: SignedId => A, g: A => Try[SignedId])(implicit secretStoreApi: SecretStoreApi):
      SignedIdEncoder[A] =
    new SignedIdEncoder[A] {
      def encode(id: SignedId): A = f(id)
      def decode(a: A): Try[SignedId] = g(a)
    }

  /**
   * A [[com.lookout.borderpatrol.sessionx.SignedIdEncoder SignedIdEncoder]] instance for
   * [[java.lang.String String]]
   */
  implicit def encodeString(implicit secretStoreApi: SecretStoreApi): SignedIdEncoder[String] = SignedIdEncoder(
    id => SignedId.toBase64(id),
    str => SignedIdInjections.str2SignedId(str)
  )

  /**
   * A [[com.lookout.borderpatrol.sessionx.SignedIdEncoder SignedIdEncoder]] instance for
   * [[com.twitter.finagle.http.Cookie Cookie]]
   */
  implicit def encodeCookie(implicit secretStoreApi: SecretStoreApi): SignedIdEncoder[Cookie] = SignedIdEncoder(
    id => id.asCookie(),
    cookie => SignedIdInjections.str2SignedId(cookie.value)
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

object SecretsEncoder {

  implicit object EncodeJson extends SecretsEncoder[Json] {

    import SecretEncoder.EncodeJson._

    //this is neccesary for it to compile otherwise it complains
    //about not knowing about the implicit object

    implicit val SecretsCodecJson: argonaut.CodecJson[Secrets] =
      casecodec2(Secrets.apply, Secrets.unapply)("current", "previous")

    def encode(s: Secrets): Json =
      s.asJson

    def decode(json: Json): Try[Secrets] = {
      json.as[Secrets].toDisjunction.fold[Try[Secrets]](
        e => Failure( SecretsDecodeError(e._1)),
        s => Success(s)
        )
    }

  }

}



