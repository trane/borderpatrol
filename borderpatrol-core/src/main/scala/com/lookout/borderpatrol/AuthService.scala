package com.lookout.borderpatrol


import com.lookout.borderpatrol.BorderPatrolApp.{RoutedRequest, Response}
import com.lookout.borderpatrol.session.{Session, MasterToken, EmptyToken}
import com.lookout.borderpatrol.session.TokenJson.{TokensJson, ServiceTokensJson}
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.io.Charsets
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._

/**
 * Created by wkimeria on 12/11/14.
 */
trait AuthResponse extends FinagleResponse
case class TokenResponse(request: RoutedRequest) extends AuthResponse {
  val httpResponse = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
}
case class LoginResponse(httpResponse: HttpResponse) extends AuthResponse

class AuthService(tokenService: Service[HttpRequest, FinagleResponse],
                  loginService: Service[RoutedRequest, FinagleResponse]) extends Service[RoutedRequest, AuthResponse] {

  def apply(request: RoutedRequest) = {
    println("----------------------------- AuthService------------------------------>")
    val r = request.session.tokens.master match {
      case t: MasterToken => getServiceTokens(request, t) map (rrequest => TokenResponse(rrequest))
      case _ => getLoginPage(request) map (response => LoginResponse(response))
    }
    println("<----------------------------- AuthService------------------------------")
    r
  }

  def getServiceTokens(request: RoutedRequest, token: MasterToken): Future[RoutedRequest] = {
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

  def getLoginPage(request: RoutedRequest): Future[FinagleResponse] =
    loginService(request)

}
