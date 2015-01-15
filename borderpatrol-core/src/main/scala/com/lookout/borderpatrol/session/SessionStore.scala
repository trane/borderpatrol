package com.lookout.borderpatrol.session

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
  private [this] var _store = Map[Seq[Byte], Seq[Byte]]()

  def cryptKey(id: String): Try[CryptKey] =
    id.asSessionIdAndSecret map (t => CryptKey(t._1, t._2))

  def cryptKey(id: SessionId): Try[CryptKey] =
    id.asSessionIdAndSecret map (t => CryptKey(t._1, t._2))

  def get(id: String): Option[Session] =
    for {
      (sid, sec) <- id.asSessionIdAndSecret.toOption
      enc <- _store get sid.signature
      session <- toSession(cryptKey(sid, sec).decrypt(enc))
    } yield session

  def toSession(bytes: Seq[Byte]): Option[Session] =
    ???

  def toBytes(s: Session): Array[Byte] =
    ???

  def get(id: SessionId): Option[Session] =
    get(id.asString)

  def update(s: Session): Session =
    s.id.asSessionIdAndSecret.toOption map ( t => {
      _store = _store.updated(s.id.signature, cryptKey(t._1, t._2).encrypt(toBytes(s)))
      s
    }) get
}
