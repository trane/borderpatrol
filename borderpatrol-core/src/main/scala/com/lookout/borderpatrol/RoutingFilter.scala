package com.lookout.borderpatrol

import com.twitter.finagle.http.service.{RoutingService => FinagleRoutingService}
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.finagle.{Filter, Service}
import org.jboss.netty.handler.codec.http._

class RoutingFilter extends Filter[HttpRequest, FinagleResponse, RoutedRequest, FinagleResponse] {

  def apply(request: HttpRequest, service: Service[RoutedRequest, FinagleResponse]) = {
    println(request.getUri + "-------- RoutingFilter ------------------------------>")
    val rRequest = RoutedRequest(request, serviceName(request.getUri))
    println( "Session type " + rRequest.session)
    println(" Service is >" + rRequest.service + "<")
    val r = service(rRequest)
    println("<----------------------------- RoutingFilter ------------------------------")
    r
  }

  //TODO: use pattern matching + Regex
  def serviceName(uri: String): String = uri match {
    case "/foo" => "foo"
    case "/foo/good" => "foo"
    case "/bar" => "bar"
    case "/baz" => "baz"
    case _ => "unknown"
  }
}


