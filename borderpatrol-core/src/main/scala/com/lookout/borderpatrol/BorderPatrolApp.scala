package com.lookout.borderpatrol

import java.io.FileReader
import java.net.InetSocketAddress

import com.twitter.finagle._
import com.twitter.finagle.builder.{ClientBuilder, ServerBuilder}
import com.twitter.finagle.http.service.RoutingService
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse, RichHttp}
import com.twitter.finagle.loadbalancer.HeapBalancerFactory
import com.twitter.server.TwitterServer
import com.twitter.util.Await
import com.typesafe.config.ConfigFactory
import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse}

import scala.collection.JavaConversions._

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
    val server = ServerBuilder()
      .codec(Http())
      .bindTo(new InetSocketAddress(8080))
      .name("BorderPatrol-1")
      .build(orchestratorService)

    //Run Mock Services
    runMockServices

    Await.ready(adminHttpServer)
  }

  /**
   * Run Mock Services
   */
  def runMockServices: Unit = {

    val router = RoutingService.byPath[HttpRequest] {
      case "/foo" => new FooService(" group 1")
      case "/foo/" => new FooService(" group 1")
      case "/foo/good" => new FooService(" group 1")
      case "/a" => authPipeline
      case "/a/" => authPipeline
    }

    val server = ServerBuilder()
      .codec(RichHttp[RoutedRequest](Http()))
      .bindTo(new InetSocketAddress(8081))
      .name("BorderPatrol-2")
      .build(router)

  }
}
