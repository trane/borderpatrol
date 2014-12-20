package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp.{Response, NeedsAuthResponse}
import com.twitter.finagle.Service
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}

/**
 * Created by wkimeria on 12/10/14.
 */
class UpstreamService extends Service[HttpRequest, FinagleResponse] {
  def apply(request: HttpRequest) = {
    println("----------------------------- UpstreamService ------------------------------>")
    var it = request.headers().entries().iterator()
    while(it.hasNext){
      println(it.next().toString)
    }
    println("HEADERS --------> " + request.headers().toString)
    val r =
      if(request.getUri() == "/good"){
        val result: Response = new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK))
        Future.value(result)
      }else{
        val result: FinagleResponse = new NeedsAuthResponse(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.UNAUTHORIZED))
        Future.value(result)
      }
    println("<----------------------------- UpstreamService ------------------------------")
    r
  }
}
