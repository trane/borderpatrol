package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp.{Response, RoutedRequest}
import com.twitter.finagle.Service
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}


/**
 * Created by wkimeria on 12/11/14.
 */
class LoginService extends Service[RoutedRequest, Response] {
  def apply(request: RoutedRequest) = {
    println("----------------------------- LoginService------------------------------>")
    val r = Future.value(new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)))
    println("<----------------------------- LoginService------------------------------")
    r
  }
}
