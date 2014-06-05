package com.lookout.borderpatrol.sessions

import com.twitter.finagle.Service
import com.twitter.finagle.SimpleFilter
import com.twitter.finagle.Service
import com.twitter.finagle.SimpleFilter
import org.jboss.netty.handler.codec.http._
import scala.collection.immutable.{Map, HashMap}
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

class SessionStoreFilter(sessionStore: SessionStore) extends SimpleFilter[HttpRequest, HttpResponse] {
  def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]) = {
    val serviceName = request.getHeader("SERVICE_NAME")
    val sessionId = request.getHeader("BORDER_PATROL_SESSION_ID")
    val session = sessionStore.session(sessionId)
    session.serviceToken map { _ get serviceName } map {
      request.setHeader("Auth-Token", _)
    }

    System.out.println("Auth-Token: %s".format(request.getHeader("Auth-Token")))
    service(request)
  }
}
