package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp._
import com.lookout.borderpatrol.session.{NewSession, SecureSession}
import com.twitter.finagle.http.service.{NullService, RoutingService => FinagleRoutingService}
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.finagle.{Filter, Service}
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._

class RoutingFilter extends Filter[HttpRequest, FinagleResponse, RoutedRequest, FinagleResponse] {

  import com.lookout.borderpatrol.session.SecureSession._

  def apply(request: HttpRequest, service: Service[RoutedRequest, FinagleResponse]) = {
    println(request.getUri + "-------- RoutingFilter ------------------------------>")
    //TODO: Testing using Finagle's RoutingService
    //Return a function that returns a service. For now, just return the same service unless unmapped
    val svc = FinagleRoutingService.byPath {
      case "/mtp" => service
      case "/mtp/good" => service
      case "/mtp/bad" => service
    }

    val rRequest = RoutedRequest(request, serviceName(request.getUri), NewSession(request))
    println(" Service is >" + rRequest.service + "<")
    val r = svc(rRequest)
    println("<----------------------------- RoutingFilter ------------------------------")
    r
  }
  def serviceName(uri: String): String = "mtp"
}


