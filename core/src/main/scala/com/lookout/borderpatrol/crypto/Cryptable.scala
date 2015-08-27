package com.lookout.borderpatrol.crypto

import com.lookout.borderpatrol.sessionx.{SessionDataError, SessionDataEncoder, Session, SessionId}
import com.twitter.io.Buf

import scala.util.Try

/**
 * Derive [[com.lookout.borderpatrol.crypto.CryptKey CryptKey]] instances to
 * encrypt [[com.lookout.borderpatrol.sessionx.Session Session]] data
 */
trait Encryptable[A] {
  implicit val toArr: A => Array[Byte]

  def encrypt(session: Session[A]): Array[Byte] =
    CryptKey(session).encrypt[A](session.data)
}

/**
 * Derive [[com.lookout.borderpatrol.crypto.CryptKey CryptKey]] instances to
 * decrypt [[com.lookout.borderpatrol.sessionx.Session Session]] data
 */
trait Decryptable[A] {
  implicit val fromArr: Array[Byte] => A

  def decrypt(id: SessionId, bytes: Array[Byte]): Try[Session[A]] =
    CryptKey(id).decrypt[A](bytes).map(Session(id, _))
}

/**
 * Default instances of Encryptable type classes for Buf and A => Buf
 */
object Encryptable {
  /* Buf => Array[Byte] */
  implicit val encryptableBuf = new Encryptable[Buf] {
    implicit val toArr: Buf => Array[Byte] = buf => Buf.ByteArray.Owned.extract(buf)
  }

  /**
   * Given a A => Buf, we can transform that into an A => Array[Byte] for any A
   * with a [[com.lookout.borderpatrol.sessionx.SessionDataEncoder SessionDataEncoder]] instance
   */
  implicit def anyBytes[A](implicit ev: SessionDataEncoder[A]): Encryptable[A] =
    new Encryptable[A] {
      implicit val toArr: A => Array[Byte] = a => encryptableBuf.toArr(ev.encode(a))
    }
}

/**
 * Default instances of Decryptable type classes for Buf and A => Buf
 */
object Decryptable {
  /* Array[Byte] => Buf */
  implicit val decryptableBuf = new Decryptable[Buf] {
    implicit val fromArr: Array[Byte] => Buf = arr => Buf.ByteArray.Owned(arr)
  }

  /**
   * Given a Buf => A, we can transform that into an Array[Byte] => A for any A
   * with a [[com.lookout.borderpatrol.sessionx.SessionDataEncoder SessionDataEncoder]] instance
   */
  implicit def anyBytes[A](implicit ev: SessionDataEncoder[A]): Decryptable[A] =
    new Decryptable[A] {
      implicit val fromArr: Array[Byte] => A = arr => ev.decode(Buf.ByteArray.Owned(arr)).get
    }
}
