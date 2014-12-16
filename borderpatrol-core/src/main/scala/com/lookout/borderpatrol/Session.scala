package com.lookout.borderpatrol

import java.util.concurrent.TimeUnit

import com.lookout.borderpatrol.SecureSessionId.SessionId
import com.twitter.util.Duration

import scala.util.{Try, Success}


object SessionExpiry extends Expiry {
  val lifetime = Duration(1, TimeUnit.DAYS)
}

sealed trait Session {
  val id: SessionId
  val req: Any
}

object Session {
  def apply(req: Any): Session = NewSession(req)

  def apply(s: String, req: Any): Session =
    SessionIdSerializer.decode(s) match {
      case Success(id) => ExistingSession(id, req)
      case _ => NewSession(req)
    }

  case class NewSession(req: Any) extends Session {
    lazy val id = SessionIdGenerator.next
  }

  case class ExistingSession(id: SessionId, req: Any) extends Session
}

/**
 * This prototypes an API, and should be implemented using some shared store.
 *
 * No coordination is needed for the store, but, should be implemented using an HA
 * store.
 */
object SessionStore {
  type Store = Map[String, SessionId]

  private [this] var _store = Map[String, SessionId]()

  def get(s: String): Option[SessionId] =
    _store.get(s) match {
      case Some(id) if id.expired => None
      case any => any
    }

  def update(id: SessionId): Try[SessionId] = {
    _store = _store.updated(id.repr, id)
    Success(id)
  }
}
