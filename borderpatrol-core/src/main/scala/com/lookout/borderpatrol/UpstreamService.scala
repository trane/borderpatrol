package com.lookout.borderpatrol

import com.twitter.finagle.http.{Http => FinagleHttp, Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.finagle.{Dtab, Path, Service}
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._

/**
 * Generic upstream service
 * @param authService
 */
class UpstreamService(authService: Service[RoutedRequest, HttpResponse],
                      upstreams: Map[String,Service[HttpRequest, HttpResponse]]) extends Service[HttpRequest, FinagleResponse] {
  def apply(request: HttpRequest) = {
    println("------------------------------ UpstreamService " + request.getUri + "----------------------------->")

    //TODO: Rewrite url
    val originalUri = request.getUri
    val mappedUrl = getRewrittenUrl(originalUri)

    //TODO: Change this to take a routed request in order to get the service name. HardCoded for now
    val clientOpt = upstreams.get("foo")

    val r = clientOpt match {
      case Some(svc) => {
        svc(request) map { resp =>
          resp.getStatus match {
            case HttpResponseStatus.UNAUTHORIZED => {
              println("returning a 401")
              NeedsAuthResponse(resp)
            }
            case _ => {
              println("returning a " + resp.getStatus.toString)
              new Response(resp)
            }
          }
        }
      }
      case None => Future.value(new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND)))
    }
    println("<----------------------------- UpstreamService ------------------------------")
    r
  }

  def getRewrittenUrl(url:String):String = {
    val dTab: Dtab = Dtab.read("/mtp=>/")
    dTab.lookup(Path.read(url)).sample().show
  }
}
