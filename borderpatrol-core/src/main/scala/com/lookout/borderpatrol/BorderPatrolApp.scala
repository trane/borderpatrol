package com.lookout.borderpatrol

import java.net.{InetAddress, InetSocketAddress}

import com.twitter.finagle.builder.{Server, ServerBuilder}
import com.twitter.server.TwitterServer
import org.jboss.netty.handler.codec.http._
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}

object BorderPatrolApp extends TwitterServer {

  //TODO: Flesh this out
  class Session {
    def token(name: String): Option[String] = {
      val tokens = Map("flexd" -> "DEADBEEF", "mtp" -> "LIVEKALE")
      tokens.get(name)
    }
  }

  //A Request with Routing and Session Information
  class RoutedRequest(val httpRequest: HttpRequest, val session: Session)
    extends FinagleRequest {
    override val remoteSocketAddress: InetSocketAddress = new InetSocketAddress(InetAddress.getLoopbackAddress, 0) //TODO: This is wrong
  }

  //Unsuccessful Response
  case class NeedsAuthResponse (httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse

  //Successful response
  case class Response (httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse


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