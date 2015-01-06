package com.lookout.borderpatrol


import com.lookout.borderpatrol.session.MasterToken
import com.lookout.borderpatrol.session.TokenJson.{ServiceTokensJson, TokensJson}
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.io.Charsets
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._

trait AuthResponse extends FinagleResponse
case class TokenResponse(request: RoutedRequest) extends AuthResponse {
  val httpResponse = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
}
case class LoginResponse(httpResponse: HttpResponse) extends AuthResponse

class AuthService(tokenService: Service[HttpRequest, FinagleResponse],
                  loginService: Service[RoutedRequest, FinagleResponse]) extends Service[RoutedRequest, FinagleResponse] {

  def apply(request: RoutedRequest): Future[AuthResponse] = {
    println("----------------------------- AuthService------------------------------>")
    val r = request.session.tokens.master match {
      case MasterToken(t) => getServiceTokens(request, t) map (rrequest => TokenResponse(rrequest))
      case _ => loginService(request) map (response => TokensJson(response.getContentString()) match {
          case Some(tokens) => TokenResponse(request ++ tokens)
          case _ => LoginResponse(response)
        })
    }
    println("<----------------------------- AuthService------------------------------")
    r
  }

  def getServiceTokens(request: RoutedRequest, token: String): Future[RoutedRequest] = {
    val url = "/api/auth/service/v1/account_token.json"
    val tokenRequest = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.POST, url)
    tokenService(tokenRequest) map { response =>
      val json = response.getContent.toString(Charsets.Utf8)
      ServiceTokensJson(json).foldRight(request)((tokens, req) => req ++ tokens)
    }
  }

  def getMasterAndServiceTokens(request: RoutedRequest): Future[RoutedRequest] = {
    val url = "/api/auth/service/v1/account_master_token.json"
    val tokenRequest = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.POST, url)
    tokenService(tokenRequest) map { response =>
      val json = response.getContent.toString(Charsets.Utf8)
      TokensJson(json).foldRight(request)((tokens, req) => req ++ tokens)
    }
  }
}
