package com.lookout.borderpatrol.session.store

import com.lookout.borderpatrol.Session
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

  def cryptKey(id: SessionId, secret: Secret): CryptKey =
    CryptKey(id, secret)

  def encrypt(s: Session, c: CryptKey): A
  def decrypt(a: A, c: CryptKey): Option[Session]
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

  def decrypt(buf: Buf, c: CryptKey): Option[Session] = {
    val encBytes = toArray(buf)
    val decBytes = c.decrypt(encBytes)
    ((json2bytes invert decBytes) toOption) flatMap (_ asSession)
  }

  def encrypt(s: Session, c: CryptKey): Buf = {
    val bytes = json2bytes (s asJson)
    val encBytes = c.encrypt(bytes)
    toBuf(encBytes)
  }

  def get(s: String): Option[Session] =
    for {
      (sid, sec) <- s.asSessionIdAndSecret.toOption
      buf <- Await.result(store.get(bytes264(sid.signature.toArray)), timeout)
      session <- decrypt(buf, cryptKey(sid, sec))
    } yield session

  def update(s: Session): Session =
    s.id.asSessionIdAndSecret.toOption map {t =>
      val encKey = cryptKey(t._1, t._2)
      val value = encrypt(s, encKey)
      val key = bytes264(s.id.signature.toArray)
      set(key, s.id.expires, value)
      s
    } get

  def cryptKey(id: String): Try[CryptKey] =
    id.asSessionIdAndSecret map (t => CryptKey(t._1, t._2))

  def cryptKey(id: SessionId): Try[CryptKey] =
    id.asSessionIdAndSecret map (t => CryptKey(t._1, t._2))

  def toArray(buf: Buf): Array[Byte] =
    Buf.ByteArray.Owned.extract(buf)

  def toBuf(bytes: Array[Byte]): Buf =
    Buf.ByteArray.Owned(bytes)

  def set(key: String, expiry: Time, value: Buf): Future[Unit] =
    store.set(key, 0, expiry, value)
      .onFailure(err => println(s"Failed ${err}"))
      .onSuccess(suc => println(s"Success ${key}"))

}

case class InMemoryEncryptedSessionStore(implicit marshaller: Marshaller) extends SessionStoreApi with EncryptedSessions[Buf] {
  private [this] lazy val json2bytes = Injection.connect[String, Array[Byte]]
  private [this] lazy val bytes264 = Injection.connect[Array[Byte], Base64String, String]
  private [this] var _store = Map[String, Buf]()

  def decrypt(buf: Buf, c: CryptKey): Option[Session] = {
    val encBytes = toArray(buf)
    val decBytes = c.decrypt(encBytes)
    ((json2bytes invert decBytes) toOption) flatMap (_ asSession)
  }

  def encrypt(s: Session, c: CryptKey): Buf = {
    val bytes = json2bytes (s asJson)
    val encBytes = c.encrypt(bytes)
    toBuf(encBytes)
  }

  def get(s: String): Option[Session] =
    for {
      (sid, sec) <- s.asSessionIdAndSecret.toOption
      buf <- _store.get(bytes264(sid.signature.toArray))
      session <- decrypt(buf, cryptKey(sid, sec))
    } yield session

  def update(s: Session): Session =
    cryptKey(s.id) map { encKey =>
      val value = encrypt(s, encKey)
      val key = bytes264(s.id.signature.toArray)
      _store = _store.updated(key, value)
      s
    } get

  def cryptKey(id: SessionId): Try[CryptKey] =
    id.asSessionIdAndSecret map (t => CryptKey(t._1, t._2))

  def toArray(buf: Buf): Array[Byte] =
    Buf.ByteArray.Owned.extract(buf)

  def toBuf(bytes: Array[Byte]): Buf =
    Buf.ByteArray.Owned(bytes)

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
