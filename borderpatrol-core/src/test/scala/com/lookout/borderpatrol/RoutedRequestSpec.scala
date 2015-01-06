package com.lookout.borderpatrol

import com.lookout.borderpatrol.session._
import org.jboss.netty.handler.codec.http._
import org.scalatest.{FlatSpec, Matchers}

class RoutedRequestSpec extends FlatSpec with Matchers {

  implicit val marshaller = Session.marshaller
  def serviceName = "testservice"
  def mockHttpRequest = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "localhost")
  def mockSessionStore = Session.sessionStore
  def mockSession = Session.newSession(mockHttpRequest)
  def mockTokens = Tokens(EmptyToken, EmptyServiceTokens)
  def mockServiceTokens = EmptyServiceTokens + ServiceToken(serviceName, "test")

  behavior of "RoutedRequest"

  it should "give the value of the border patrol cookie if present" in {
    val req = mockHttpRequest
    val session = mockSession
    req.headers.add(HttpHeaders.Names.COOKIE, s"${Session.cookieName}=${session.id.asString}")
    val rReq = RoutedRequest(req, serviceName)
    rReq.borderCookie.get shouldBe session.id.asString
    RoutedRequest(mockHttpRequest, serviceName).borderCookie shouldBe None
  }

  it should "create a new session if a valid session doesn't exist" in {
    val req = RoutedRequest(mockHttpRequest, serviceName)
    mockSessionStore.get(req.session.id) shouldBe None
  }

  it should "reuse an existing session if it exists" in {
    val session = mockSession
    mockSessionStore.update(session)
    val req = mockHttpRequest
    req.headers.add(HttpHeaders.Names.COOKIE, s"${Session.cookieName}=${session.id.asString}")
    val rReq = RoutedRequest(req, serviceName)
    rReq.session shouldBe session
  }

  it should "give the same RoutedRequest replacing an existing session with added session" in {
    val req = RoutedRequest(mockHttpRequest, serviceName)
    val newSession = mockSession
    val updatedReq = req += newSession
    updatedReq.session should not be req.session
    updatedReq.session shouldBe newSession
    updatedReq.httpRequest shouldBe req.httpRequest
    updatedReq.service shouldBe req.service
  }

  it should "give the same RoutedRequest replacing an existing master token with added master token" in {
    val req = RoutedRequest(mockHttpRequest, serviceName)
    val master = MasterToken("test")
    val updatedReq = req + master
    updatedReq.session.id shouldBe req.session.id
    updatedReq.session.tokens.master shouldBe master
    updatedReq.httpRequest shouldBe req.httpRequest
    updatedReq.service shouldBe req.service
  }

  it should "give the same RoutedRequest converging existing service tokens with added service token" in {
    val req = RoutedRequest(mockHttpRequest, serviceName)
    val serviceToken1 = ServiceToken(serviceName, "test")
    val serviceToken2 = ServiceToken(serviceName, "newtestvalue")
    val updatedReq = req + serviceToken1
    updatedReq.session.id shouldBe req.session.id
    updatedReq.session.tokens.master shouldBe req.session.tokens.master
    updatedReq.session.tokens.service(serviceName).get shouldBe serviceToken1
    (updatedReq + serviceToken2).session.tokens.service(serviceName).get shouldBe serviceToken2
    updatedReq.httpRequest shouldBe req.httpRequest
    updatedReq.service shouldBe req.service
  }

  it should "give the same RoutedRequest converging existing service tokens with added service tokens" in {
    val req = RoutedRequest(mockHttpRequest, serviceName) ++ mockServiceTokens
    val serviceTokens = mockServiceTokens + ServiceToken(serviceName, "testing")
    val updatedReq = req ++ serviceTokens
    updatedReq.session.id shouldBe req.session.id
    updatedReq.session.tokens.master shouldBe req.session.tokens.master
    updatedReq.session.tokens.service(serviceName).get shouldBe serviceTokens.get(serviceName).get
    updatedReq.httpRequest shouldBe req.httpRequest
    updatedReq.service shouldBe req.service
  }

  it should "give the same RoutedRequest converging existing tokens with added tokens" in {
    val req = RoutedRequest(mockHttpRequest, serviceName)
    val masterToken = MasterToken("test")
    val serviceTokens = mockServiceTokens
    val tokens = Tokens(masterToken, serviceTokens)
    val updatedReq = req ++ tokens
    updatedReq.session.id shouldBe req.session.id
    updatedReq.session.tokens.master shouldBe masterToken
    updatedReq.session.tokens.service(serviceName).get shouldBe serviceTokens.get(serviceName).get
    updatedReq.httpRequest shouldBe req.httpRequest
    updatedReq.service shouldBe req.service
  }

  it should "give the same RoutedRequest with empty tokens when clearTokens is called" in {
    val tokens = Tokens(MasterToken("test"), mockServiceTokens)
    val req = RoutedRequest(mockHttpRequest, serviceName) ++ tokens
    req.session.tokens shouldBe tokens
    req.clearTokens.session.tokens shouldBe Tokens.empty
  }
}