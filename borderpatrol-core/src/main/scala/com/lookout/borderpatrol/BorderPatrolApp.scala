package com.lookout.borderpatrol

import java.net.{InetAddress, InetSocketAddress}

import com.lookout.borderpatrol.session._
import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.service.RoutingService
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse, RichHttp, HttpMuxer, Http}
import com.twitter.finagle.{Filter, _}
import com.twitter.server.TwitterServer
import com.twitter.util.Await
import org.jboss.netty.handler.codec.http.{HttpResponse, HttpRequest}

object BorderPatrolApp extends TwitterServer {

  val loginPipeline = new LoginFilter andThen new LoginService
  val authService = new AuthService(new TokenService, loginPipeline)
  val sessionFilter = new SessionFilter
  val upstreamService = new UpstreamService(authService)
  val upstreamFilter = new UpstreamFilter(authService)
  val routingFilter = new RoutingFilter


  val basePipeline = routingFilter andThen sessionFilter
  val orchestratorService = basePipeline andThen upstreamFilter andThen upstreamService

  val authPipeline = basePipeline andThen authService
  val upstreamPipeline = basePipeline andThen upstreamFilter


  def main() {

    val router = RoutingService.byPath[HttpRequest] {
      case "/mtp" => new MtpService
      case "/mtp/" => new MtpService
      case "/a" => authPipeline
      case "/a/" => authPipeline
    }

    val server = ServerBuilder()
      .codec(Http())
      .bindTo(new InetSocketAddress(8080))
      .name("BorderPatrol-1")
      .build(orchestratorService)

    val server2 = ServerBuilder()
      .codec(RichHttp[RoutedRequest](Http()))
      .bindTo(new InetSocketAddress(8081))
      .name("BorderPatrol-2")
      .build(router)

    Await.ready(adminHttpServer)
  }
}
