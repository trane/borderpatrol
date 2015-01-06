package com.lookout.borderpatrol.session

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