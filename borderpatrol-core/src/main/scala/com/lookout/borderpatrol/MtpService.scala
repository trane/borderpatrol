package com.lookout.borderpatrol

/**
 * Created by wkimeria on 12/23/14.
 */

import java.nio.charset.Charset

import com.lookout.borderpatrol.BorderPatrolApp.{NeedsAuthResponse, Response}
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.util.Future
import org.jboss.netty.buffer.{ChannelBuffers, ChannelBuffer}
import org.jboss.netty.handler.codec.http._

/**
 * Created by wkimeria on 12/23/14.
 */
class MtpService extends Service[HttpRequest, HttpResponse] {
  def apply(request: HttpRequest) = {
    println("------------------------------ MtpService " + request.getUri + " ----------------------------->")
    val r = Future.value(
      request.getUri match {
        case "/mtp/good" => new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK))
        case _ => {
          println(" Token = " + request.headers().get("Auth-Token"))
          if (request.headers().get("Auth-Token")!= null && request.headers().get("Auth-Token").contains("DEADLAKE")){
            val resp = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
            val cb: ChannelBuffer = ChannelBuffers.copiedBuffer("I am a Teapot from the MTP Service", Charset.defaultCharset())
            resp.setContent(cb)
            resp
          }else{
            new  DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.UNAUTHORIZED)
          }
        }
      }
    )
    println("<----------------------------- MtpService ------------------------------")
    r
  }

}