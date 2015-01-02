package com.lookout.borderpatrol

import com.lookout.borderpatrol.session.SecureSession
import com.twitter.finagle.http.{Cookie, Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.finagle.{Service, SimpleFilter}
import org.jboss.netty.handler.codec.http.{Cookie => NettyCookie}


/**
 * Ensure the set-cookie header is set on the response
 */
class SessionFilter extends SimpleFilter[RoutedRequest, FinagleResponse] {
  implicit val marshaller = SecureSession.marshaller

  def apply(request: RoutedRequest, service: Service[RoutedRequest, FinagleResponse]) = {
    println("------------------------------ SessionFilter ----------------------------->")
    val r = service(request) map (response => responseWithCookie(response)(request))
    println("<------------------------------ SessionFilter -----------------------------")
    r
  }

  /**
   * Set the cookie header on the response if not set or needs to be replaced
   * @param response The response to set the cookie header
   * @param request The request that has the session and cookie values
   * @return The response with set-cookie header if needed
   */
  def responseWithCookie(response: FinagleResponse)(request: RoutedRequest): FinagleResponse = {
    val id = request.session.id.asString
    request.cookies.get(SecureSession.cookieName) match {
      case Some(c) if c.value == id => response
      case _ => addCookie(response, id)
    }
  }

  /**
   * In-place add the set-cookie header to the response
   * @param response
   * @param value The session id string
   */
  def addCookie(response: FinagleResponse, value: String): FinagleResponse = {
    response.cookies += createCookie(value)
    response
  }


  def createCookie(value: String): Cookie = {
    val cookie = new Cookie(SecureSession.cookieName, value)
    //cookie.isSecure = true
    cookie.maxAge = SecureSession.lifetime
    //cookie.domain = "lookout.com"
    cookie
  }

}
