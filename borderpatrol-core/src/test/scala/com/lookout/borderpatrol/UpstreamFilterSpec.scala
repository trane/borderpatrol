package com.lookout.borderpatrol

import com.twitter.finagle.{Filter, Service}
import com.twitter.util.{Future, Await}
import org.jboss.netty.handler.codec.http._
import org.scalatest.{Matchers, FlatSpec}
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}

/**
 * Created by wkimeria on 12/12/14.
 */
class UpstreamFilterSpec extends FlatSpec with Matchers{
  def mockUpstreamService(response: FinagleResponse) = new Service[HttpRequest, FinagleResponse] {
    def apply(request: HttpRequest) = Future.value(response)
  }

  def MockTokenService(response: FinagleResponse) = new Service[HttpRequest, FinagleResponse] {
    def apply(request: HttpRequest) = Future.value(response)
  }

  def mockLoginService(response: FinagleResponse) = new Service[RoutedRequest, FinagleResponse] {
    def apply(request: RoutedRequest) = Future.value(new LoginResponse(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)))
  }

  def httpReq = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/good")
  def httpRep = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
  def mockRequest = RoutedRequest(httpReq, "foo")
  def mockRequestWithMasterSession = RoutedRequest(httpReq, "foo")
  def mockResponse = FinagleResponse(httpRep)

  def mockUpService200  = new Service [HttpRequest, FinagleResponse]{
    def apply(request: HttpRequest) = {
      Future.value(new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)))
    }
  }

  def mockUpService401  = new Service [HttpRequest, FinagleResponse]{
    def apply(request: HttpRequest) = {
      Future.value(new NeedsAuthResponse(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.UNAUTHORIZED)))
    }
  }

 //TODO: Test that the first 401 redirects to AuthService

  //TODO: Test that with a Master Token and Auth Service mocked to return service token, we get a 200 from Upstream


  "An UpstreamFilter" should "route to Upstream service successfully" in {
    Await.result(
      new UpstreamFilter(
        new AuthService(MockTokenService(mockResponse),
          mockLoginService(mockResponse))).apply(mockRequest, mockUpService200)) shouldBe a [Response]
  }

  "An UpstreamFilter" should "return login page" in {
    Await.result(
      new UpstreamFilter(
        new AuthService(MockTokenService(mockResponse),
          mockLoginService(mockResponse))).apply(mockRequest, mockUpService401)) shouldBe a [LoginResponse]
  }
}
