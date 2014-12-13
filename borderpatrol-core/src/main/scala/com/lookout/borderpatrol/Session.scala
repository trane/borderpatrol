package com.lookout.borderpatrol
import com.lookout.borderpatrol.SecureSessionId.SessionId
//import com.twitter.finagle.http.Request

import scala.util.Success

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
