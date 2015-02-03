package com.lookout.borderpatrol.session.store

import com.lookout.borderpatrol.Session
import com.lookout.borderpatrol.session.id.Types.Signature
import com.lookout.borderpatrol.session.id._
import com.lookout.borderpatrol.session._
import com.twitter.bijection.{Base64String, Injection}
import com.twitter.finagle.Memcachedx
import com.twitter.finagle.memcachedx
import com.twitter.io.Buf
import com.twitter.util.{Time, Future, Duration, Await}
import com.lookout.borderpatrol.util.Combinators.tap

import scala.util.Try

trait SessionStoreComponent {
  implicit val marshaller: Marshaller
  val sessionStore: SessionStoreApi
}
/**
 * This prototypes an API, and should be implemented using some shared store.
 *
 * No coordination is needed for the store, but, should be implemented using an HA
 * store.
 */
sealed trait SessionStoreApi {
  def get(s: String): Option[Session]
  def update(s: Session): Session
}

sealed trait EncryptedSessions[A] {
  def decrypt(a: A, c: CryptKey): Option[Session]
  def encrypt(s: Session, c: CryptKey): A
}

/**
 * Memcached encrypted store, utilizing Ketama consistent hashing
 *
 * @param dest A comma separated value list of host:port, localhost:11211,localhost:11212
 * @param timeout The timeout for fetching from memcache
 * @param marshaller A session marshaller
 */
case class MemcachedEncryptedSessionStore(dest: String, timeout: Duration)(implicit marshaller: Marshaller) extends SessionStoreApi with EncryptedSessions[Buf] {
  val store: memcachedx.BaseClient[Buf] = Memcachedx.newKetamaClient(dest)
  val flag = 0 // ignored flag required by memcached api

  def decrypt(buf: Buf, c: CryptKey): Option[Session] =
    c.decrypt(buf.asArray).asSession

  def encrypt(s: Session, c: CryptKey): Buf =
    c.encrypt(s.asBytes).asBuf

  def get(s: String): Option[Session] =
    for {
      (sid, sec) <- s.asSessionIdAndSecret.toOption
      key <- sid.deriveCryptKey
      buf <- get(sid)
      session <- decrypt(buf, key)
    } yield session

  def update(s: Session): Session =
    s.id.deriveCryptKey map {key =>
      tap(s)(s_ => set(s.id, encrypt(s, key)))
    } get

  private[this] def get(id: SessionId): Option[Buf] =
    Await.result(store.get(id.signature.asBase64), timeout)

  private[this] def set(id: SessionId, value: Buf): Future[Unit] =
    store.set(id.signature.asBase64, flag, id.expires, value)
      .onFailure(err => println(s"Failed ${err}"))
      .onSuccess(suc => println(s"Success ${id}"))

}

case class InMemoryEncryptedSessionStore(implicit marshaller: Marshaller) extends SessionStoreApi with EncryptedSessions[Buf] {
  private [this] var _store = Map[String, Buf]()

  def decrypt(buf: Buf, c: CryptKey): Option[Session] =
    c.decrypt(buf.asArray).asSession

  def encrypt(s: Session, c: CryptKey): Buf =
    c.encrypt(s.asBytes).asBuf

  def get(s: String): Option[Session] =
    for {
      (sid, sec) <- s.asSessionIdAndSecret.toOption
      buf <- _store.get(sid.signature.asBase64)
      session <- decrypt(buf, CryptKey(sid, sec))
    } yield session

  def update(s: Session): Session =
    s.id.deriveCryptKey map { encKey =>
      val value = encrypt(s, encKey)
      val key = s.id.signature.asBase64
      _store = _store.updated(key, value)
      s
    } get
}

case class InMemorySessionStore(implicit marshaller: Marshaller) extends SessionStoreApi {
  private [this] var _store = Map[String, Session]()

  def get(id: String): Option[Session] = {
    (_store get id) filterNot (_.id.expired)
  }

  def get(id: SessionId): Option[Session] =
    get(id.asString)

  def update(s: Session): Session = {
    _store = _store.updated(s.id.asString, s)
    s
  }
}

case class MemcachedSessionStore(dest: String, timeout: Duration)(implicit marshaller: Marshaller) extends SessionStoreApi {
  val store = Memcachedx.newKetamaClient(dest).withStrings

  def get(s: String): Option[Session] = {
    for {
      res <- Await.result(store.get(s).liftToTry, timeout).toOption
      json <- res
      session <- json.asSession
      if !session.id.expired
    } yield session
  }

  def get(s: Session): Option[Session] =
    get(s.id.asString)

  def update(s: Session): Session =
    tap(s)(s => Await.result(store.set(s.id.asString, 0, s.id.expires, s.asJson)))

  def updated(s: Session): Future[Unit] =
    store.set(s.id.asString, 0, s.id.expires, s.asJson)
}
