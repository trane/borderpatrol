package com.lookout.borderpatrol.routers

import com.twitter.bijection.twitter_util.UtilBijections
import com.twitter.finagle.httpx.Request
import com.twitter.util.Future
import io.finch.request._
import com.lookout.borderpatrol.sessionx._

/**
 * A collection of RequestReader[A] types and functions to interact with requests
 * coming in to Border Patrol
 */
class readers {

  implicit val sessionIdDecoder: DecodeRequest[SessionId] =
    DecodeRequest[SessionId](s => UtilBijections.twitter2ScalaTry.inverse(SessionId.from[String](s)))

  val sessionIdReader: RequestReader[SessionId] =
    cookie("border_session").map(_.value).as[SessionId]

  def sessionReader(store: SessionStore): RequestReader[Session[Request]] =
    sessionIdReader.embedFlatMap(store.get[Request]).embedFlatMap {
      case Some(s) => Future.value(s)
      case None => Future.exception(new RequestError("invalid session"))
    }

}
