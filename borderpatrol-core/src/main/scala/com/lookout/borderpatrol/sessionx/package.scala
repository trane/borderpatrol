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

import com.twitter.finagle.httpx.Cookie
import com.twitter.io.Buf
import com.twitter.util.{Base64StringEncoder, Time, Future}
import com.twitter.finagle.memcachedx

import scala.collection.mutable
import scalaz.{\/-, -\/}

/**
 * This crypto introduces types and functions that enable identifying, fetching, and storing web session data. This
 * is accomplished by a set of types that will be used by consumers of this library: `Session`, `Store`, and `Secret`.
 *
 * A [[Secret]] is a cryptographically verifiable signing key used to sign a [[SessionId]]. Creating a `Secret` is
 * simple. It defaults to expire at [[Secret.lifetime]]
 *
 * {{{
 *   val secret = Secret() // default secret expiry
 *   val expiringSecret = Secret(Time.now)
 *
 *   val randomBytes = EntropyGenerator(16) // 16 bytes of randomness
 *   val randomId = EntropyGenerator(1).head // 1 byte of randomness for an id
 *   val expiry = Time.from(0) // very expired
 *   val constructedSecret = Secret(randomId, randomBytes, expiry)
 *   log(s"secret has expired: ${constructedSecret.expired == true}")
 *
 *   val signedMsg = secret.sign("message to by signed".getBytes)
 * }}}
 *
 * A [[SessionId]] is a cryptographically signed identifier for a [[Session]], it consists of entropy, expiry, secret,
 * and signature of those items. This is meant to be used as the [[com.twitter.finagle.httpx.Cookie]] value, so we
 * provide serializing to [[String]].
 *
 * {{{
 *   val id: SessionId = Await.result(SessionId.next)
 *   val cookieValue: String = id.asBase64
 *   SessionId.from[String](cookieValue) == id
 * }}}
 *
 * A [[Session]] is product type of a cryptographically verifiable identifier [[SessionId]] and an arbitrary data type
 * A`. The only requirement for a [[SessionStore]][B,M] to store/fetch a `Session[A]` is that there be some implicit
 * injective views from `A %> B` and `B %> Option[A]`.
 *
 * We have provided default views for: `httpx.Request %> Buf`, `Json %> Buf` and their injective views.
 *
 * {{{
 *  // set up secret/session stores
 *  implicit val secretStore = SecretStores.InMemorySecretStore(Secrets(Secret(), Secret()))
 *  val sessionStore = SessionStores.InMemoryStore()
 *
 *  // create a Session[httpx.Request]
 *  val newSessionFuture = Session(Request("http://localhost/api/stuff")) // entropy is blocking on the JVM
 *  val newSession = Await.result(newSessionFuture)
 *
 *  // see if the session expired (checks the [[SessionId.expires]])
 *  log(s"Session has expired? ${newSession.expired}")
 *
 *  // store the session and then fetch it
 *  sessionStore.update(newSession).onFailure(log)
 *  sessionStore.get(newSession.id).onSuccess(s => s match {
 *    case Some(s) => log(s"Same session?: ${newSession == s}")
 *    case None => log("hrm, where did the session go?")
 *  })
 * }}}
 *
 * Let's say you have a [[Session.data]] type that doesn't have the injective [[View]] that you need, that's OK!
 * Assuming you are storing it in memcached, which requires a type of [[Buf]] for the value:
 *
 * {{{
 *   trait Foo {
 *     val value: Int
 *   }
 *
 *   implicit val foo2Int: Foo %> Buf = View(f => Buf.U32BE(f.value))
 *   implicit val int2OptFoo: Buf %> Option[Foo] = View(b => new Foo { override val value = Buf.U32BE.unapply(b) })
 *
 *   val foo1 = new Foo { override val value = 1 }
 *   val fooSession = Session(foo1)
 *   sessionStore.update(fooSession)
 * }}}
 *
 */
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
      crypto.CryptKey(s).encrypt(f(s))
  }


  /**
   * Default implementations of [[SecretStoreApi]]
   */
  object SecretStores {

    /**
     * A useful [[Secrets]] mock store for quickly testing and prototyping
     *
     * @param secrets the current secret and previous secret
     */
    case class InMemorySecretStore(secrets: Secrets) extends SecretStoreApi {
      @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.Var")) // this is for mocking
      private[this] var _secrets: Secrets = secrets

      def current: Secret = {
        val c = _secrets.current
        if (c.expired) {
          _secrets = _secrets.copy(Secret(), c)
          _secrets.current
        }
        else c
      }

      def previous: Secret =
        _secrets.previous

      def find(f: (Secret) => Boolean): Option[Secret] =
        if (f(current)) Some(current)
        else if (f(previous)) Some(previous)
        else None
    }

  }


  /**
   * Default implementations of [[SessionStore]] with [[memcachedx]] and an in-memory store for mocking
   */
  object SessionStores {

    /**
     * Memcached backend to [[SessionStore]]
     *
     * {{{
     *   val store = MemcachedStore(Memcachedx.newKetamaClient("localhost:11211"))
     *   val requestSession = store.get[httpx.Request](id) // default views from `Buf` %> `Request` are included
     *   requestSession.onSuccess(s => log(s"Success! you were going to ${s.data.uri}"))
     *                 .onFailure(log)
     * }}}
     * @param store finagle [[memcachedx.BaseClient]] memcached backend
     */
    case class MemcachedStore(store: memcachedx.BaseClient[Buf])
        extends SessionStore[Buf, memcachedx.BaseClient[Buf]] {
      val flag = 0 // ignored flag required by memcached api

      /**
       * Fetches a [[Session]] if one exists otherwise `None`. On failure will make a [[Future.exception]].
       *
       * @param key lookup key
       * @param ev evidence for converting the Buf to the type of A
       * @tparam A [[Session.data]] type that must have a view from `Buf %> Option[A]`
       *
       * @return
       */
      def get[A](key: SessionId)(implicit ev: Session[Buf] %> Option[Session[A]]): Future[Option[Session[A]]] =
        store.get(key.asBase64).map { obuf =>
          obuf.flatMap { buf =>
            ev(Session(key, buf))
          }
        }

      /**
       * Stores a [[Session]]. On failure returns a [[Future.exception]]
       *
       * @param session
       * @param ev evidence for the conversion to `Buf`
       * @tparam A [[Session.data]] type that must have a view from `A %> Buf`
       *
       * @return a [[Future]]
       */
      def update[A](session: Session[A])(implicit ev: Session[A] %> Session[Buf]): Future[Unit] =
        store.set(session.id.asBase64, flag, session.id.expires, ev(session).data)
    }

    /**
     * An in-memory store for prototyping and testing remote stores
     */
    @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.MutableDataStructures"))
    case object InMemoryStore
        extends SessionStore[Buf, mutable.Set[Session[Buf]]] {

      val store: mutable.Set[Session[Buf]] = mutable.Set[Session[Buf]]()

      def get[A](key: SessionId)(implicit ev: Session[Buf] %> Option[Session[A]]): Future[Option[Session[A]]] =
        store.find(_.id == key).flatMap(s => ev(s)).toFuture

      def update[A](session: Session[A])(implicit ev: Session[A] %> Session[Buf]): Future[Unit] =
        if (store.add(ev(session)))
          Future.Unit
        else
          Future.exception[Unit](new UpdateStoreException(s"Unable to update store with $session"))
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
