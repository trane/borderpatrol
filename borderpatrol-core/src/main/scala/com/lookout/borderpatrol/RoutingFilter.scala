package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp._
import com.lookout.borderpatrol.session.{SecureSession, NewSession}
import com.twitter.finagle.{Filter, Service}
import com.twitter.util.{Future}
import org.jboss.netty.handler.codec.http._
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}

class RoutingFilter extends Filter[HttpRequest, FinagleResponse, RoutedRequest, FinagleResponse] {
  import SecureSession._

  def apply(request: HttpRequest, service: Service[RoutedRequest, FinagleResponse]) = {
    println(request.getUri + "-------- RoutingFilter ------------------------------>")
    val r =
      if(request.getUri() == "/notfound" || request.getUri() == "/favicon.ico"){
        val result: FinagleResponse = new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND))
        Future.value(result)
      }else{
        val serviceName = "mtp"
        val rRequest = RoutedRequest(request, serviceName, NewSession(request))
        service(rRequest)
      }
    println("<----------------------------- RoutingFilter ------------------------------")
    r
  }
}


