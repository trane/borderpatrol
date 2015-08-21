package com.lookout.borderpatrol.server

import com.lookout.borderpatrol.sessionx.SecretStoreApi
import com.twitter.bijection.twitter_util.UtilBijections
import com.twitter.finagle.httpx
import com.twitter.util.Future
import io.finch.request._

/**
 * A collection of RequestReader[A] types and functions to interact with requests
 * coming in to Border Patrol
 */
object readers {
  import com.lookout.borderpatrol.sessionx._

  implicit def sessionIdDecoder(implicit secretStoreApi: SecretStoreApi): DecodeRequest[SessionId] =
    DecodeRequest[SessionId](s => UtilBijections.twitter2ScalaTry.inverse(SessionId.from[String](s)))

  val sessionIdReader: RequestReader[SessionId] =
    cookie("border_session").map(_.value).as[SessionId]

  def sessionReader(store: SessionStore): RequestReader[Session[httpx.Request]] =
    sessionIdReader.embedFlatMap(store.get[httpx.Request]).embedFlatMap {
      case Some(s) => Future.value(s)
      case None => Future.exception(new RequestError("invalid session"))
    }

}
