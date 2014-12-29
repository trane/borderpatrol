package com.lookout.borderpatrol

/**
 * The UpstreamFilter is responsible for calling the UpstreamsService and returning non 401 responses
 * back up the chain. For 401 responses, it calls an optional Service in line to get an Authorization
 * token for the upstream service that returned the 401. If that succeeds, it makes a call back to
 * the service once more with the Authorization Token
 */

import com.twitter.finagle.{Filter, SimpleFilter, Service}
import com.twitter.util.{Future, Await}
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}


/**
 * Converges all of the requirements for the receiving service
 *
 * @param auth The auth service, to be called on an initial 401
 */
class UpstreamFilter(auth: Service[RoutedRequest, FinagleResponse]) extends Filter[RoutedRequest, FinagleResponse, FinagleRequest, FinagleResponse] {

  def apply(request: RoutedRequest, service: Service[FinagleRequest, FinagleResponse]): Future[FinagleResponse] = {
    println("------------------------------ UpstreamFilter ----------------------------->")

    val r = service(request) flatMap (firstResponse => {firstResponse match {
      case NeedsAuthResponse(_) => auth(request) flatMap (loginOrAuth => {loginOrAuth match {
        case TokenResponse(req) => service(req) flatMap (lastResponse => lastResponse match {
          case NeedsAuthResponse(_) => auth(request.clearTokens)
          case _ => Future.value(lastResponse)
        })
        case LoginResponse(_) => Future.value(loginOrAuth)
      }})
      case _ => Future.value(firstResponse)
    }})
    println("<------------------------------ UpstreamFilter -----------------------------")
    r
  }
}
