package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit

import com.twitter.util.Duration

import scala.util.{Success, Try}


object SessionExpiry extends Expiry {
  val lifetime = Duration(1, TimeUnit.DAYS)
}

sealed trait Session {
  val id: SessionId
  val req: Any
}

case class NewSession(req: Any)(implicit g: SessionIdGenerator, s: SecretStoreApi) extends Session {
  lazy val id = g.next
}

case class ExistingSession(id: SessionId, req: Any) extends Session

/**
 * This prototypes an API, and should be implemented using some shared store.
 *
 * No coordination is needed for the store, but, should be implemented using an HA
 * store.
 */
sealed trait SessionStoreApi {
  def get(s: String): Option[SessionId]
  def update(id: SessionId): Try[SessionId]
}

trait SessionStoreComponent {
  implicit val marshaller: SessionIdMarshaller
  val sessionStore: SessionStoreApi
}

case class InMemorySessionStore(implicit marshaller: SessionIdMarshaller) extends SessionStoreApi {
  private [this] var _store = Map[String, SessionId]()

  def get(s: String): Option[SessionId] =
    _store.get(s) match {
      case Some(id) if id.expired => None
      case any => any
    }

  def update(id: SessionId): Try[SessionId] = {
    _store = _store.updated(id.asString, id)
    Success(id)
  }
}
