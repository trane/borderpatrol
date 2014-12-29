package com.lookout.borderpatrol

import com.lookout.borderpatrol.session.TokenJson.TokensJson
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.finagle.{Service, Filter}
import com.twitter.util.Future
import com.twitter.io.Charsets
import org.jboss.netty.handler.codec.http.{HttpResponse, HttpMethod}

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
    TokensJson(json).foldRight(request)((tokens, req) => req ++ tokens)
  }
}
