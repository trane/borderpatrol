package com.lookout.borderpatrol.session.store

import com.lookout.borderpatrol.Session
import com.lookout.borderpatrol.session.id._
import com.lookout.borderpatrol.session._
import com.twitter.bijection.{Base64String, Injection}
import com.twitter.finagle.Memcached
import com.twitter.io.Charsets
import com.twitter.util.{Future, Duration, Await}
import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers}
import com.lookout.borderpatrol.util.Combinators.tap

import scala.util.Try

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

trait EncryptedSessions {
  def cryptKey(id: String): Try[CryptKey]
  def cryptKey(id: SessionId): Try[CryptKey]
  def cryptKey(id: SessionId, secret: Secret): CryptKey =
    CryptKey(id, secret)
}

trait SessionStoreComponent {
  implicit val marshaller: Marshaller
  val sessionStore: SessionStoreApi
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

case class InMemoryEncryptedSessionStore(implicit marshaller: Marshaller) extends SessionStoreApi with EncryptedSessions {
  private [this] lazy val json2bytes = Injection.connect[String, Array[Byte]]
  private [this] lazy val bytes264 = Injection.connect[Array[Byte], Base64String, String]
  private [this] var _store = Map[Seq[Byte], String]()

  def cryptKey(id: String): Try[CryptKey] =
    id.asSessionIdAndSecret map (t => CryptKey(t._1, t._2))

  def cryptKey(id: SessionId): Try[CryptKey] =
    id.asSessionIdAndSecret map (t => CryptKey(t._1, t._2))

  def get(id: String): Option[Session] =
    for {
      (sid, sec) <- id.asSessionIdAndSecret.toOption
      base64 <- _store get sid.signature
      bytes <- toBytes(base64)
      json <- json2bytes.invert(cryptKey(sid, sec).decrypt(bytes).toArray).toOption
      session <- json.asSession
    } yield session

  def toBytes(s: Session): Array[Byte] =
    json2bytes(s.asJson)

  def toBytes(s: String): Option[Array[Byte]] =
    bytes264.invert(s).toOption

  def get(id: SessionId): Option[Session] =
    get(id.asString)

  def update(s: Session): Session =
    s.id.asSessionIdAndSecret.toOption map (t => {
      val key = cryptKey(t._1, t._2)
      val encrypted = key.encrypt(toBytes(s))
      val encoded = bytes264(encrypted.toArray)
      _store = _store.updated(s.id.signature, encoded)
      s
    }) get

}

case class MemcachedSessionStore(dest: String, timeout: Duration)(implicit marshaller: Marshaller) extends SessionStoreApi {
  val store = Memcached.newRichClient(dest).withStrings

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
