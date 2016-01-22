package com.lookout.borderpatrol.crypto

import com.lookout.borderpatrol.sessionx.{EncryptedDataEncoder, SessionDataEncoder, SignedId, Session}
import com.twitter.io.Buf
import com.twitter.util.Future
import com.twitter.finagle.memcached

import scala.util.{Success, Failure}

/**
 * Session store that will store encrypted `Session[_]` data into a backend
 * TODO: This is silly, let's remove this EncryptedDataEncoder type and treat Session as a Functor, apply the encryption
 * and decryption with mixins into the base SessionStore
 */
trait EncryptedSessionStore {
  def update[A](session: Session[A])(implicit ev: EncryptedDataEncoder[A]): Future[Unit]
  def get[A](key: SignedId)(implicit ev: EncryptedDataEncoder[A]): Future[Option[Session[A]]]
}

/**
 * Default implementations of [[com.lookout.borderpatrol.crypto.EncryptedSessionStore EncryptedSessionStore]] with
 * [[com.twitter.finagle.memcached memcached]] and an in-memory store for mocking
 */
object EncryptedSessionStore {

  /**
   * Memcached backend to [[com.lookout.borderpatrol.crypto.EncryptedSessionStore EncryptedSessionStore]]
   *
   * {{{
   *   val store = MemcachedStore(Memcached.newKetamaClient("localhost:11211"))
   *   val requestSession = store.get[http.Request](id)
   *   requestSession.onSuccess(s => log(s"Success! you were going to ${s.data.uri}"))
   *                 .onFailure(log)
   * }}}
   * @param store finagle [[com.twitter.finagle.memcached.BaseClient memcached.BaseClient]] memcached backend
   */
  case class MemcachedStore(store: memcached.BaseClient[Buf])
      extends EncryptedSessionStore {
    val flag = 0 // ignored flag required by memcached api

    /**
     * Fetches a [[com.lookout.borderpatrol.sessionx.Session Session]] if one exists otherwise `None`. On failure
     * will make a [[com.twitter.util.Future.exception Future.exception]].
     *
     * @param key lookup key
     * @param ev evidence for converting the Buf to the type of A
     * @tparam A [[Session.data]] type that must be decryptable
     *
     * @return
     */
    def get[A](key: SignedId)(implicit ev: EncryptedDataEncoder[A]): Future[Option[Session[A]]] =
      store.get(key.asBase64).flatMap(_ match {
        case None => Future.value(None)
        case Some(buf) => ev.decrypted(key, buf) match {
          case Failure(e) => Future.exception[Option[Session[A]]](e)
          case Success(session) => Future.value(Some(session))
        }
      })

    /**
     * Stores a [[com.lookout.borderpatrol.sessionx.Session Session]]. On failure returns a
     * [[com.twitter.util.Future.exception Future.exception]]
     *
     * @param session
     * @param ev evidence for the conversion to `Buf`
     * @tparam A [[Session.data]] type that must have be encryptable
     *
     * @return a [[com.twitter.util.Future Future]]
     */
    def update[A](session: Session[A])(implicit ev: EncryptedDataEncoder[A]): Future[Unit] =
      store.set(session.id.asBase64, flag, session.id.expires, ev.encrypted(session))
  }
}
