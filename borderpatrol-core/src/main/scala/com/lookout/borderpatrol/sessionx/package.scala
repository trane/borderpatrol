/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Lookout, Inc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package com.lookout.borderpatrol

import com.twitter.io.Buf
import com.twitter.util.{Base64StringEncoder, Time, Future}
import com.twitter.finagle.memcachedx

import scala.collection.mutable

package object sessionx extends SessionFunctions {

  implicit class SecretOps(val s: Secret) extends AnyVal {
    def expired: Boolean =
      s.expiry < Time.now || s.expiry > Secret.currentExpiry
  }

  implicit class SessionIdOps(val id: SessionId) extends AnyVal {
    def payload: Payload =
      SessionId.payload(id)

    def signWith(s: Secret): Signature =
      SessionId.signWith(id, s)

    def expired: Boolean =
      SessionId.expired(id)

    def as[A](implicit f: SessionId => A): A =
      SessionId.as[A](id)(f)

    def asSeq: IndexedSeq[Byte] =
      SessionId.toIndexedSeq(id)

    def asArray: Array[Byte] =
      SessionId.toArray(id)

    def asBase64: String =
      SessionId.toBase64(id)
  }

  implicit class SessionOps[A](val s: Session[A]) extends AnyVal {
    def as[B](implicit f: Session[A] => B): B =
      f(s)

    def expired: Boolean =
      s.id.expired

    def encrypt(implicit f: Session[A] => Array[Byte]): Array[Byte] =
      CryptKey(s).encrypt(f(s))
  }



  object SecretStores {

    case class InMemorySecretStore(secrets: Secrets) extends SecretStoreApi {
      private[this] var _secrets: Secrets = secrets

      def current = {
        val c = _secrets.current
        if (c.expired) {
          _secrets = _secrets.copy(Secret(), c)
          _secrets.current
        }
        else c
      }

      def previous =
        _secrets.previous

      def find(f: (Secret) => Boolean) =
        if (f(current)) Some(current)
        else if (f(previous)) Some(previous)
        else None
    }

  }


  object SessionStores {

    case class MemcachedStore(store: memcachedx.BaseClient[Buf])(
        implicit i2s: SessionId %> String,
                 s2b: PSession %> Buf,
                 b2s: Buf %> Option[PSession])
        extends SessionStore[memcachedx.BaseClient[Buf]] {
      val flag = 0 // ignored flag required by memcached api

      def update(key: SessionId)(value: PSession): Future[Unit] =
        store.set(i2s(key), flag, key.expires, s2b(value))

      def get(key: SessionId): Future[Option[PSession]] =
        store.get(i2s(key)) map (_ flatMap (b2s(_)))
    }

    case class InMemoryStore(store: mutable.Map[String, PSession] = mutable.Map[String, PSession]())(
        implicit i2s: SessionId => String)
        extends SessionStore[mutable.Map[String, PSession]] {

      def update(key: SessionId)(value: PSession) =
        store.put(i2s(key), value).
            fold(Future.exception[Unit](new
                UpdateStoreException(s"Unable to update store with $value")))(_ => Future.value[Unit](()))

      def get(key: SessionId) =
        store.get(i2s(key)).filterNot(session => session.id.expired).toFuture

    }

    /*
    implicit object SessionEncryptor extends Encryptable[PSession, SessionId, Array[Byte]] {
      def apply(a: PSession)(key: SessionId): Array[Byte] =
        CryptKey(key).encrypt[PSession](a)
    }

    implicit object SessionDecryptor extends Decryptable[Array[Byte], SessionId, PSession] {
      def apply(e: Array[Byte])(key: SessionId): PSession =
        CryptKey(key).decryptAs[PSession](e)
    }

    implicit object SessionCryptographer extends SessionCrypto[Array[Byte]]

    case class EncryptedInMemorySessionStore(
          store: mutable.Map[String, Encrypted] = mutable.Map[String, Encrypted]())(
        implicit i2s: SessionId %> String)
        extends EncryptedSessionStore[Encrypted, mutable.Map[String, Encrypted]] {

      def update(key: SessionId)(value: PSession) =
        store.put(i2s(key), value.encrypt).
            fold(Future.exception[Unit](new
                UpdateStoreException(s"Unable to update store with $value")))(_ => Future.value[Unit](()))

      def get(key: SessionId) =
        store.get(i2s(key)).flatMap(decrypt(_)).filterNot(session => session.id.expired).toFuture
    }
    */
  }

}
