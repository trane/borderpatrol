package com.lookout.borderpatrol.server

import com.twitter.bijection.twitter_util.UtilBijections
import com.twitter.util.Future
import io.finch.request._

/**
 * A collection of RequestReader[A] types and functions to interact with requests
 * coming in to Border Patrol
 */
object readers {
  import com.lookout.borderpatrol.sessionx._

  implicit def sessionIdDecoder(implicit secretStoreApi: SecretStoreApi): DecodeRequest[SessionId] =
    DecodeRequest[SessionId](str =>
      UtilBijections.twitter2ScalaTry.inverse( // convert to twitter Try
        SessionId.from[String](str)
      )
    )

  val sessionIdReader: RequestReader[SessionId] =
    cookie("border_session").map(_.value).as[SessionId]

  def sessionReader[A](store: SessionStore): RequestReader[Session[A]] =
    sessionIdReader.embedFlatMap(store.get[A]).embedFlatMap {
      case Some(s) => Future.value(s)
      case None => Future.exception(new SessionError("invalid session"))
    }

}
