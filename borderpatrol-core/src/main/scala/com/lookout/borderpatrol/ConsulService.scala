package com.lookout.borderpatrol

import com.twitter.finagle.Service
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._

/**
 * Call Consul Service endPoint
 */
class ConsulService extends Service[HttpRequest, HttpResponse] {
  def apply(request: HttpRequest) = {
    println("------------------------------ ConsulService----------------------------->")
    upstreams.get("consul") map (svc =>
      svc(request) filter (_.getStatus == HttpResponseStatus.OK)
      ) getOrElse (Future.value(Responses.NotFound()))
  }
}