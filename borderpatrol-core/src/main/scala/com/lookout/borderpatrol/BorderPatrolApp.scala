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
import com.typesafe.config.ConfigFactory

object BorderPatrolApp extends TwitterServer {

  val loginPipeline = new LoginFilter andThen new LoginService
  val sessionFilter = new SessionFilter
  val authService = sessionFilter andThen new AuthService(new TokenService, loginPipeline)
  val upstreamService = new UpstreamService(authService)
  val upstreamFilter = new UpstreamFilter(authService)
  val routingFilter = new RoutingFilter


  val basePipeline = routingFilter andThen sessionFilter
  val orchestratorService = basePipeline andThen upstreamFilter andThen upstreamService

  val authPipeline = basePipeline andThen authService
  val upstreamPipeline = basePipeline andThen upstreamFilter

  def main() {

    val conf = ConfigFactory.load("borderpatrol.conf")

    val server = ServerBuilder()
      .codec(Http())
      .bindTo(new InetSocketAddress(8080))
      .name("BorderPatrol-1")
      .build(orchestratorService)

    //Run Mock Services
    runMockServices

    Await.ready(adminHttpServer)
  }

  def runMockServices(): Unit ={

    val router1 = RoutingService.byPath[HttpRequest] {
      case "/mtp" => basePipeline andThen new MtpService(" group 1")
      case "/mtp/" => basePipeline andThen new MtpService(" group 1")
      case "/a" => authPipeline
      case "/a/" => authPipeline
    }

    val router2 = RoutingService.byPath[HttpRequest] {
      case "/mtp" => basePipeline andThen new MtpService(" group 2")
      case "/mtp/" => basePipeline andThen new MtpService(" group 2")
      case "/a" => authPipeline
      case "/a/" => authPipeline
    }

    val server2 = ServerBuilder()
      .codec(RichHttp[RoutedRequest](Http()))
      .bindTo(new InetSocketAddress(8081))
      .name("BorderPatrol-2")
      .build(router1)

    val server3 = ServerBuilder()
      .codec(RichHttp[RoutedRequest](Http()))
      .bindTo(new InetSocketAddress(8082))
      .name("BorderPatrol-3")
      .build(router2)

  }
}
