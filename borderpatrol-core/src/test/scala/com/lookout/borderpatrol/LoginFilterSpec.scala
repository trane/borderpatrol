package com.lookout.borderpatrol

import com.lookout.borderpatrol.session.{EmptyToken, Session}
import com.twitter.finagle.http.{Cookie, Response => FinagleResponse}
import com.twitter.util.Await
import org.jboss.netty.handler.codec.http._
import org.scalatest.{FlatSpec, Matchers}

class LoginFilterSpec extends FlatSpec with Matchers {
  def loginReq = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.POST, "/a/login")
  def httpReq = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/a")
  def httpRep = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
  val serviceName = "foo"
  def mockLoginRequest = RoutedRequest(loginReq, serviceName)
  def mockRequest = RoutedRequest(httpReq, "bar")
  def mockResponse = FinagleResponse(httpRep)
  def mockCookie(value: String): Cookie = new Cookie(Session.cookieName, value)
  val mockTokenService = new TokenService
  val mockLoginService = new LoginService
  def filter = new LoginFilter

  behavior of "LoginFilter"

  it should "parse responses from Login requests" in {
    val resp = filter(mockLoginRequest, mockTokenService)
    Await.result(resp) shouldBe a [TokenResponse]
  }

  it should "contain tokens from valid Login requests" in {
    val resp = filter(mockLoginRequest, mockTokenService)
    Await.result(resp) match {
      case TokenResponse(req) => (req.serviceToken getOrElse EmptyToken) should not be EmptyToken
      case _ => fail("Not a token response")
    }
  }

  it should "create a login response for all other requests" in {
    val resp = filter(mockRequest, mockLoginService)
    Await.result(resp) shouldBe a [LoginResponse]
  }
}
