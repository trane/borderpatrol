package com.lookout.borderpatrol

import java.net.{InetAddress, InetSocketAddress}

import com.lookout.borderpatrol.session._
import com.twitter.finagle.builder.{Server, ServerBuilder}
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse, HttpMuxer, Http}
import com.twitter.server.TwitterServer
import org.jboss.netty.handler.codec.http._

object BorderPatrolApp extends TwitterServer {

  //A Request with Routing and Session Information
  case class RoutedRequest(httpRequest: HttpRequest, service: String, session: Session) extends FinagleRequest {
    override val remoteSocketAddress: InetSocketAddress = new InetSocketAddress(InetAddress.getLoopbackAddress, 0) //TODO: This is wrong
    def +(s: Session): RoutedRequest = copy(httpRequest, service, s)
    def +(t: Option[SessionTokens]) = copy(httpRequest, service, ExistingSession(session.id, session.originalRequest, t))
    def serviceToken: Option[ServiceToken] = {
      println("t.service = " + session.tokens)
      println("service = " + service)
      for(t <- session.tokens; st <- t.service(service)) yield st
    }
    def toHttpRequest: HttpRequest = {
      addAuthHeaders
      addBorderHeaders
      httpRequest
    }
    /* TODO: make this more functional */
    def addAuthHeaders: Unit =
      serviceToken.foreach(t => httpRequest.headers.add("Auth-Token", t.value))
    /* TODO: make this more functional */
    def addBorderHeaders: Unit =
      httpRequest.headers.add("Via", "Border Patrol")
  }

  //Unsuccessful Response
  case class NeedsAuthResponse(httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse

  //Successful response
  case class Response(httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse

  val sessionFilter = new SessionFilter
  var authService = new AuthService
  val upstreamService = new UpstreamService(authService)
  val upstreamFilter = new UpstreamFilter(authService)
  val routingFilter = new RoutingFilter
  val loginFilter = new LoginFilter
  val loginService = new LoginService

  val orchestratorService = routingFilter andThen sessionFilter andThen upstreamFilter andThen upstreamService


  def main() {



    val server: Server = ServerBuilder()
      .codec(Http())
      .bindTo(new InetSocketAddress(8080))
      .name("BorderPatrol-1")
      .build(orchestratorService)

    HttpMuxer.addHandler("/mtp", new MtpService)
    HttpMuxer.addHandler("/mtp/", new MtpService)

    val server2: Server = ServerBuilder()
      .codec(Http())
      .bindTo(new InetSocketAddress(8081))
      .name("BorderPatrol-2")
      .build(HttpMuxer)
  }
}
