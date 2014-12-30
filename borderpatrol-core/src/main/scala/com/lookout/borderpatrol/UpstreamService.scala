package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp.{NeedsAuthResponse, Response}
import com.twitter.finagle.{Http, Service}
import com.lookout.borderpatrol.BorderPatrolApp.{RoutedRequest, NeedsAuthResponse, Response}
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.loadbalancer.{HeapBalancerFactory, DefaultBalancerFactory}
import com.twitter.finagle.service.SingletonFactory
import com.twitter.finagle.{Dtab, Http, Service, Path}
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse, Http => FinagleHttp}
import com.twitter.util.{Await, Future}
import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse, HttpResponseStatus}

/**
 * Generic upstream service
 * @param authService
 */
class UpstreamService(authService: Service[RoutedRequest, HttpResponse]) extends Service[HttpRequest, FinagleResponse] {
  def apply(request: HttpRequest) = {
    println("------------------------------ UpstreamService " + request.getUri + "----------------------------->")

    val mappedUrl = getRewrittenUrl("/mtp/good")
    println("MAPPED URL **********************> " + mappedUrl)

    //TODO: Figure out the correct way of load balancing
    val client = ClientBuilder()
      .codec(FinagleHttp())
      .hosts("localhost:8081,localhost:8082")
      .hostConnectionLimit(1)
      .loadBalancer(HeapBalancerFactory.toWeighted)
      .retries(2)
      .build()

    val r = client(request) map { resp =>
      resp.getStatus match {
        case HttpResponseStatus.UNAUTHORIZED => {
          println("returning a 401")
          NeedsAuthResponse(resp)
        }
        case _ => new Response(resp)
      }
    }
    println("<----------------------------- UpstreamService ------------------------------")
    r
  }

  def getRewrittenUrl(url:String):String = {
    val dTab: Dtab = Dtab.read("/mtp=>/")
    dTab.lookup(Path.read(url)).sample().show
  }
}
