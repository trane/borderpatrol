package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp.{Response, NeedsAuthResponse}
import com.twitter.finagle.Service
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}

/**
 * Created by wkimeria on 12/10/14.
 */
class UpstreamService(authService: Service[HttpRequest, HttpResponse]) extends Service[HttpRequest, FinagleResponse] {
  def apply(request: HttpRequest) = {
    println("------------------------------ UpstreamService ----------------------------->")
    request.headers.entries.asInstanceOf[List].foreach(println(_))
    val r = Future.value(
      request.getUri match {
        case "/good" => new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK))
        case _ => new NeedsAuthResponse(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.UNAUTHORIZED))
      }
    )
    println("<----------------------------- UpstreamService ------------------------------")
    r
  }

}
