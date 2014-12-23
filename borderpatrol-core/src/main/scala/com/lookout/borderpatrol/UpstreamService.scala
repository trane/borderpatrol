package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp.{NeedsAuthResponse, Response}
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._

/**
 * Created by wkimeria on 12/10/14.
 */
class UpstreamService(authService: Service[HttpRequest, HttpResponse]) extends Service[HttpRequest, FinagleResponse] {
  def apply(request: HttpRequest) = {
    println("------------------------------ UpstreamService ----------------------------->")
    //request.headers.entries.asInstanceOf[List[Map[String, String]]].foreach(println(_))
    val r = Future.value(
      request.getUri match {
        case "/good" => new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK))
        case _ => {
          println(" Token = " + request.headers().get("Auth-Token"))
          if (request.headers().get("Auth-Token")!= null && request.headers().get("Auth-Token").contains("DEADLAKE")){
            val resp = new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK))
            resp.setContentString("I am a Teapot")
            resp
          }else{
            new NeedsAuthResponse(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.UNAUTHORIZED))
          }

        }
      }
    )
    println("<----------------------------- UpstreamService ------------------------------")
    r
  }

}
