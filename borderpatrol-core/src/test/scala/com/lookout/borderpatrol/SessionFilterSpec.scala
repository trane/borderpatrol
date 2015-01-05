package com.lookout.borderpatrol

import session.Session
import util.Combinators.tap
import com.twitter.finagle.Service
import com.twitter.util.{Await, Future}
import org.jboss.netty.handler.codec.http._
import org.scalatest.{Matchers, FlatSpec}
import com.twitter.finagle.http.{Cookie, Response => FinagleResponse}

class SessionFilterSpec extends FlatSpec with Matchers {
  import Session.marshaller

  def mockUpstreamService(response: FinagleResponse) = new Service[RoutedRequest, FinagleResponse] {
    def apply(request: RoutedRequest) = Future.value(response)
  }

  def httpReq = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/good")
  def httpRep = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
  def mockRequest = RoutedRequest(httpReq, "mtp")
  def mockResponse = FinagleResponse(httpRep)
  def mockCookie(value: String): Cookie = new Cookie(Session.cookieName, value)
  def filter = new SessionFilter

  def hasSetCookie: Future[FinagleResponse] => Boolean = resp =>
    Await.result(resp map (_.headerMap.get("Set-Cookie") isDefined))

  def requestWithValidCookie: RoutedRequest =
    tap(mockRequest)(r => r.cookies += mockCookie(r.session.id.asString))

  def requestWithInvalidCookie: RoutedRequest =
    tap(mockRequest)(_.cookies += mockCookie("expired"))

  behavior of "A SessionFilter"

  it should "add the Set-Cookie header if no cookie is present" in {
    val resp = filter(mockRequest, mockUpstreamService(mockResponse))
    assert(hasSetCookie(resp))
  }

  it should "add the Set-Cookie header if cookie is present, but session has expired or is invalid" in {
    val resp = filter(requestWithInvalidCookie, mockUpstreamService(mockResponse))
    assert(hasSetCookie(resp))
  }

  it should "not add the Set-Cookie header on responses from requests that already have a valid cookie" in {
    val resp = filter(requestWithValidCookie, mockUpstreamService(mockResponse))
    assert(!hasSetCookie(resp))
  }
}
