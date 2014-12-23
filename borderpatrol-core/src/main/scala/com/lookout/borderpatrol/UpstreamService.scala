package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp.{NeedsAuthResponse, Response}
import com.twitter.finagle.{Http, Service}
import com.lookout.borderpatrol.BorderPatrolApp.{RoutedRequest, NeedsAuthResponse, Response}
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.util.{Await, Future}
import org.jboss.netty.handler.codec.http._

/**
 * Created by wkimeria on 12/10/14.
 */
class UpstreamService(authService: Service[RoutedRequest, HttpResponse]) extends Service[HttpRequest, FinagleResponse] {
  def apply(request: HttpRequest) = {
    println("------------------------------ UpstreamService " + request.getUri + "----------------------------->")
    //val resp = Http.fetchUrl("https://localhost:8081/mtp" + request.getUri)
    val resp = Http.fetchUrl("https://localhost:8081/mtp")  //TODO: This needs to be the appropriate HTTP Verb
    val response = new Response(Await.result(resp))
    val modifiedResponse = response.status match {
      case HttpResponseStatus.UNAUTHORIZED => {
        println("returning a 401")
        new NeedsAuthResponse(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.UNAUTHORIZED))
      }
      case _ => response
    }
    val r = Future.value(modifiedResponse)
    println("<----------------------------- UpstreamService ------------------------------")
    r
  }

}
