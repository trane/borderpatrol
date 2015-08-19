package com.lookout.borderpatrol.routers

import com.twitter.finagle.httpx.Request
import com.twitter.util.Future
import io.finch.request._
import com.lookout.borderpatrol.sessionx._

/**
 * A collection of RequestReader[A] types and functions to interact with requests
 * coming in to Border Patrol
 */
class readers {
  val sessionIdReader: RequestReader[SessionId] =
    cookie("border_session").map(_.value).as[SessionId]

  def sessionReader(store: SessionStore[Request]): RequestReader[Session[Request]] =
    sessionIdReader.embedFlatMap(store.get[Request]).embedFlatMap {
      case Some(s) => Future.value(s)
      case None => Future.exception(new RequestError("invalid session"))
    }

}
