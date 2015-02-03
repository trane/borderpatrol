package com.lookout.borderpatrol.session.secret.watcher.consul

import com.twitter.finagle.httpx.netty.Bijections
import com.twitter.finagle.httpx.{Request, Response, Status}
import com.twitter.finagle.{Http, Service}

class ConsulService extends Service[Request, Response] {
  def apply(request: Request) = {
      Http.fetchUrl(request.uri)
        .map(Bijections.responseFromNetty(_))
        .map(resp => resp.status match {
          case Status.Ok => resp
          case _ => Response(Status.NotFound)
      })
  }
}
