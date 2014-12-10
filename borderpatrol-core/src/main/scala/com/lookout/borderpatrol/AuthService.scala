package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp.{Response}
import com.twitter.finagle.Service
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}

/**
 * Created by wkimeria on 12/11/14.
 */
class AuthService extends Service[HttpRequest, FinagleResponse] {
  def apply(request: HttpRequest) = {
    println("----------------------------- AuthService------------------------------>")
    val result: FinagleResponse = new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK))
    val r = Future.value(result)
    println("<----------------------------- AuthService------------------------------")
    r
  }
}
