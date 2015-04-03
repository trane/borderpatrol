package com.lookout.borderpatrol.session.secret.watcher.consul

import com.twitter.finagle.httpx.netty.Bijections
import com.twitter.finagle.httpx.{Request, Response, Status}
import com.twitter.finagle.{Http, Service}
import com.twitter.util.Future

class ConsulService extends Service[Request, Response] {
  def apply(request: Request): Future[Response] =
    request.location.fold(Future.value(Response(Status.BadRequest)))(
      Http.fetchUrl(_)
        .map(Bijections.responseFromNetty(_))
        .map(resp => resp.status match {
          case Status.Ok => resp
          case _ => Response(Status.NotFound)
        })
    )
}
