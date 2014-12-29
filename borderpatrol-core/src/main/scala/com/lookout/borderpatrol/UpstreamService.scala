package com.lookout.borderpatrol

<<<<<<< HEAD
import com.lookout.borderpatrol.BorderPatrolApp.{NeedsAuthResponse, Response}
=======
>>>>>>> de0f14b... Implement mock login/auth/token services
import com.twitter.finagle.{Http, Service}
import com.lookout.borderpatrol.BorderPatrolApp.{RoutedRequest, NeedsAuthResponse, Response}
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.util.{Await, Future}
import org.jboss.netty.handler.codec.http._

/**
 * Generic upstream service
 * @param authService
 */
class UpstreamService(authService: Service[RoutedRequest, HttpResponse]) extends Service[HttpRequest, FinagleResponse] {
  def apply(request: HttpRequest) = {
    println("------------------------------ UpstreamService " + request.getUri + "----------------------------->")
    //val resp = Http.fetchUrl("https://localhost:8081/mtp" + request.getUri)
    val r = Http.fetchUrl("https://localhost:8081/mtp") map { resp => //TODO: This needs to be the appropriate HTTP Verb
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
}
