package com.lookout.borderpatrol

import java.io.FileReader
import java.net.InetSocketAddress

import com.twitter.finagle._
import com.twitter.finagle.builder.{ClientBuilder, ServerBuilder}
import com.twitter.finagle.http.service.RoutingService
import com.twitter.finagle.http.{Http, RichHttp, Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.finagle.loadbalancer.HeapBalancerFactory
import com.twitter.server.TwitterServer
import com.twitter.util.Await
import com.typesafe.config.ConfigFactory
import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse}

import scala.collection.JavaConversions._

object BorderPatrolApp extends TwitterServer {

  lazy val upstreams = getUpstreamClients

  val loginPipeline = new LoginFilter andThen new LoginService
  val sessionFilter = new SessionFilter
  val authService = sessionFilter andThen new AuthService(new TokenService, loginPipeline)
  val upstreamService = new UpstreamService(authService, upstreams)
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
   * Build upstream clients from borderpatrol.conf. A map of the clients (where service name is the key)
   * gets passed to the UpstreamService, which dispatches requests based on the service name
   * @return
   */
  def getUpstreamClients: Map[String, Service[HttpRequest, HttpResponse]] = {
    val conf = ConfigFactory.parseReader(new FileReader("borderpatrol.conf"))
    val services = conf.getConfigList("services").toList
    case class ServiceConfiguration(name: String, friendlyName: String, hosts: String, rewriteRule: String) {}

    val clients = services map(s =>
      (s.getString("name"),
        ClientBuilder()
          .codec(Http())
          .hosts(s.getString("hosts"))
          .hostConnectionLimit(10)
          .loadBalancer(HeapBalancerFactory.toWeighted)
          .retries(2)
          .build()))
    clients.toMap
  }

  def runMockServices: Unit = {

    val router1 = RoutingService.byPath[HttpRequest] {
      case "/foo" => basePipeline andThen new FooService(" group 1")
      case "/foo/" => basePipeline andThen new FooService(" group 1")
      case "/a" => authPipeline
      case "/a/" => authPipeline
    }

    val router2 = RoutingService.byPath[HttpRequest] {
      case "/foo" => basePipeline andThen new FooService(" group 2")
      case "/foo/" => basePipeline andThen new FooService(" group 2")
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