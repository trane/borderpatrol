package com.lookout.borderpatrol

/**
 * The UpstreamFilter is responsible for calling the UpstreamsService and returning non 401 responses
 * back up the chain. For 401 responses, it calls an optional Service in line to get an Authorization
 * token for the upstream service that returned the 401. If that succeeds, it makes a call back to
 * the service once more with the Authorization Token
 */

import com.lookout.borderpatrol.BorderPatrolApp._
import com.lookout.borderpatrol.session.TokenJson
import com.twitter.finagle.http.{Response => FinagleResponse}
import com.twitter.finagle.{Filter, Service}
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http.HttpRequest


/**
 * Converges all of the requirements for the receiving service
 *
 * @param auth The auth service, to be called on an initial 401
 */
class UpstreamFilter(auth: Service[RoutedRequest, FinagleResponse]) extends Filter[RoutedRequest, FinagleResponse, HttpRequest, FinagleResponse] {

  def apply(request: RoutedRequest, service: Service[HttpRequest, FinagleResponse]): Future[FinagleResponse] = {
    println("------------------------------ UpstreamFilter ----------------------------->")
    val r = service(request.toHttpRequest) flatMap (firstResponse => firstResponse match {
      case NeedsAuthResponse(_) => loginResponseOrAuthRequest(request) flatMap { loginOrAuthenticated =>
        val (loginResponse, authRequest) = loginOrAuthenticated
        println(authRequest.session.tokens)
        if (authRequest.serviceToken.isDefined)
          service(authRequest.toHttpRequest) map (lastResponse => lastResponse match {
            case NeedsAuthResponse(_) => loginResponse
            case _ => lastResponse
          })
        else Future.value(loginResponse)
      }
      case _ => {
        Future.value(firstResponse)
      }
    })
    println("<------------------------------ UpstreamFilter -----------------------------")
    r
  }

  def loginResponseOrAuthRequest(request: RoutedRequest): Future[(FinagleResponse, RoutedRequest)] =
    auth(request) map (resp => (resp, request + TokenJson(resp.contentString)))
}