package com.lookout.borderpatrol

import com.lookout.borderpatrol.session.{ServiceToken, Tokens, ServiceTokens, Token}
import com.twitter.finagle.httpx.netty.Bijections
import com.twitter.finagle.httpx.{Method, Request}
import org.jboss.netty.handler.codec.http.HttpRequest

case class RoutedRequest(request: Request, service: String, session: Session) {

  def +=(otherSession: Session): RoutedRequest =
    copy(session = otherSession)

  def +(token: Token): RoutedRequest =
    this += session.copy(tokens = session.tokens += token)

  def ++(tokens: ServiceTokens): RoutedRequest =
    this += session.copy(tokens = session.tokens ++= tokens)

  def ++(tokens: Tokens): RoutedRequest =
    this += session.copy(tokens = session.tokens ++= tokens)

  def clearTokens: RoutedRequest =
    this += session.copy(tokens = Tokens.empty)

  def serviceToken: Option[ServiceToken] =
    session.tokens.service(service)

  def masterToken: Token =
    session.tokens.master

  def injectHeaders: RoutedRequest = {
    addAuthHeaders
    addBorderHeaders
    this
  }

  /* TODO: make this more functional */
  def addAuthHeaders: Unit =
    serviceToken foreach (t => request.headerMap.add("Auth-Token", t.value))

  def addBorderHeaders: Unit =
    request.headerMap.add("Via", "Border Patrol")

  def useOriginalUri: Unit =
    request.uri = session.originalRequest.getUri

  def useOriginalMethod: Unit =
    request.method = Method(session.originalRequest.getMethod.getName)

  def borderCookie: Option[String] =
    request.cookies.getValue(Session.cookieName)
}

object RoutedRequest {

  def apply(request: HttpRequest, name: String): RoutedRequest =
    RoutedRequest(Bijections.requestFromNetty(request), name)

  def apply(request: Request, name: String): RoutedRequest =
    RoutedRequest(request, name, buildSession(request))

  def buildSession(request: Request): Session =
    borderCookieValue(request).fold(Session.newSession(request))(Session(_, request))

  def borderCookieValue(request: Request): Option[String] =
    request.cookies.getValue(Session.cookieName)
}

