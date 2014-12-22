package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp.{Response, RoutedRequest}
import com.lookout.borderpatrol.session._
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse, Cookie, Http}
import org.jboss.netty.handler.codec.http.{Cookie => NettyCookie, DefaultCookie}


/**
 * Ensure the set-cookie header is set on the response
 */
class SessionFilter extends SimpleFilter[RoutedRequest, FinagleResponse] {
  val cookieName = "border_session"
  implicit val marshaller = SecureSession.marshaller

  def apply(request: RoutedRequest, service: Service[RoutedRequest, FinagleResponse]) =
    service(request) map (response => responseWithCookie(response)(request))

  /**
   * Set the cookie header on the response if not set or needs to be replaced
   * @param response The response to set the cookie header
   * @param request The request that has the session and cookie values
   * @return The response with set-cookie header if needed
   */
  def responseWithCookie(response: FinagleResponse)(request: RoutedRequest): FinagleResponse = {
    val id = request.session.id.asString
    request.cookies.getValue(cookieName).filter(_ != id).foreach(_ => setCookie(response, id))
    response
  }

  /**
   * In-place add the set-cookie header to the response
   * @param response
   * @param value The session id string
   */
  def setCookie(response: FinagleResponse, value: String): Unit = {
    val cookie = new Cookie(cookieName, value)
    cookie.isSecure = true
    cookie.domain = "lookout.com"
    response.addCookie(cookie)
  }
}
