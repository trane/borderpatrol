package com.lookout.borderpatrol

import com.lookout.borderpatrol.session.{Session, NewSession, SecureSession}
import com.twitter.finagle.http.service.{NullService, RoutingService => FinagleRoutingService}
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.finagle.{Filter, Service}
import org.jboss.netty.handler.codec.http._

class RoutingFilter extends Filter[HttpRequest, FinagleResponse, RoutedRequest, FinagleResponse] {

  def apply(request: HttpRequest, service: Service[RoutedRequest, FinagleResponse]) = {
    println(request.getUri + "-------- RoutingFilter ------------------------------>")
    val rRequest = RoutedRequest(request, serviceName(request.getUri))
    println(" Service is >" + rRequest.service + "<")
    val r = service(rRequest)
    println("<----------------------------- RoutingFilter ------------------------------")
    r
  }

  def serviceName(uri: String): String = uri match {
    case "/mtp" => "l4e"
    case "/a" => "checkpoint"
    case _ => "unknown"
  }
}


