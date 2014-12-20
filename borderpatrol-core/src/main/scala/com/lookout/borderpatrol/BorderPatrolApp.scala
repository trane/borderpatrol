package com.lookout.borderpatrol

import java.net.{InetAddress, InetSocketAddress}

import com.lookout.borderpatrol.session._
import com.twitter.finagle.builder.{Server, ServerBuilder}

import com.twitter.server.TwitterServer
import org.jboss.netty.handler.codec.http._
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}

object BorderPatrolApp extends TwitterServer {

  //A Request with Routing and Session Information
  case class RoutedRequest(httpRequest: HttpRequest, service: String, session: Session) extends FinagleRequest {
    override val remoteSocketAddress: InetSocketAddress = new InetSocketAddress(InetAddress.getLoopbackAddress, 0) //TODO: This is wrong
    def +(s: Session): RoutedRequest = copy(httpRequest, service, s)
    def serviceToken: Option[ServiceToken] = for(t <- session.tokens; st <- t.service(service)) yield st
  }

  //Unsuccessful Response
  case class NeedsAuthResponse(httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse

  //Successful response
  case class Response(httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse

  val sessionFilter = new SessionFilter
  val upstreamService = new UpstreamService
  val upstreamFilter = new UpstreamFilter(None, None)
  val routingFilter = new RoutingFilter
  val loginFilter = new LoginFilter
  val loginService = new LoginService
  var authService = new AuthService
  val authFilter = new AuthFilterCond(authService)
  val upstreamCombination = upstreamFilter andThen upstreamService
  val loginCombination = loginFilter andThen authFilter
  val upStreamFilterWithLeft = new UpstreamFilter(Some(loginCombination andThen upstreamFilter andThen upstreamService), Some(upstreamService))

  val orchestratorService = routingFilter andThen sessionFilter andThen upStreamFilterWithLeft andThen upstreamService

  def main() {
    val server: Server = ServerBuilder()
      .codec(Http())
      .bindTo(new InetSocketAddress(8080))
      .name("BorderPatrol")
      .build(orchestratorService)
  }
}