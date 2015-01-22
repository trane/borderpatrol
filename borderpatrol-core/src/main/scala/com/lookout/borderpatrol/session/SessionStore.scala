package com.lookout.borderpatrol.session

import com.twitter.bijection.{Bijection, Base64String, Injection}

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
  implicit val marshaller: SessionIdMarshaller
  val sessionStore: SessionStoreApi
}

case class InMemorySessionStore(implicit marshaller: SessionIdMarshaller) extends SessionStoreApi {
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

case class InMemoryEncryptedSessionStore(implicit marshaller: SessionIdMarshaller) extends SessionStoreApi with EncryptedSessions {
  private [this] var json2bytes = Injection.connect[String, Array[Byte]]
  private [this] var bytes264 = Injection.connect[Array[Byte], Base64String, String]
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
    s.id.asSessionIdAndSecret.toOption map ( t => {
      val encrypted = cryptKey(t._1, t._2).encrypt(toBytes(s))
      val encoded = bytes264(encrypted.toArray)
      _store = _store.updated(s.id.signature, encoded)
      s
    }) get
}
