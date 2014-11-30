package com.lookout.borderpatrol.sessions

import com.twitter.finagle.SimpleFilter
import com.twitter.finagle.Service
import org.jboss.netty.handler.codec.http._
import java.util.UUID.randomUUID
import org.jboss.netty.handler.codec.http.DefaultCookie
import scala.collection.JavaConversions._
import com.twitter.logging.Logging
import com.twitter.util.Try

class SessionIDFilter(sessionStore: SessionStore) extends SimpleFilter[HttpRequest, HttpResponse] {

  val cookieKey = "border_session"
  val maxAge = 24 * 60 * 60

  def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]) = {
    val id = getIDFromRequest(request) getOrElse generateNewID

    System.out.println("Session ID: %s".format(id))

    service(request) onSuccess (setCookie(_, id))
  }

  def generateNewID: String = {
    val id = randomUUID.toString
    sessionStore.addSession(id)
    id
  }

  def getIDFromRequest(request: HttpRequest): Option[String] = {
    Option(request.getHeader(HttpHeaders.Names.COOKIE)) flatMap {
      getIDFromValidCookie(_)
    }
  }

  def getIDFromValidCookie(cookieHeader: String): Option[String] = {
    val cookies = new CookieDecoder().decode(cookieHeader).toSet
    val cookie: Option[Cookie] = cookies find {
      validCookie(_)
    }
    cookie map {
      _.getValue
    }
  }

  def validCookie(cookie: Cookie): Boolean = {
    cookie.getName == cookieKey
  }

  def setCookie(response: HttpResponse, sessionId: String): Unit = {
    val cookie = new DefaultCookie(cookieKey, sessionId)
    cookie.setMaxAge(maxAge)
    cookie.setDomain("lookout.com")
    cookie.setSecure(true)
    response.setHeader(HttpHeaders.Names.SET_COOKIE, cookie)
  }

}
