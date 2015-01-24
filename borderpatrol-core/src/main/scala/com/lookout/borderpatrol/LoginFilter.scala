package com.lookout.borderpatrol

import com.lookout.borderpatrol.session.tokens._
import com.twitter.finagle.http.{Response => FinagleResponse}
import com.twitter.finagle.{Filter, Service}
import org.jboss.netty.handler.codec.http.HttpMethod

class LoginFilter extends Filter[RoutedRequest, AuthResponse, RoutedRequest, FinagleResponse] {
  def apply(request: RoutedRequest, service: Service[RoutedRequest, FinagleResponse]) = {
    println("----------------------------- LoginFilter------------------------------>")
    val r = service(request) map {response =>
      if (request.method == HttpMethod.POST && request.path == "/a/login")
        TokenResponse(addTokensToRequest(response, request))
      else
        LoginResponse(response)
    }
    println("<----------------------------- LoginFilter------------------------------")
    r
  }

  def addTokensToRequest(response: FinagleResponse, request: RoutedRequest): RoutedRequest = {
    val json = response.getContentString()
    json.asTokens.foldRight(request)((tokens, req) => req ++ tokens)
  }
}
