package com.lookout.borderpatrol

import java.nio.charset.Charset

import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.util.Future
import org.jboss.netty.buffer.{ChannelBuffers, ChannelBuffer}
import org.jboss.netty.handler.codec.http._

class FooService(group: String) extends Service[HttpRequest, FinagleResponse] {

  def apply(request: HttpRequest) = {
    println("------------------------------ FooService " + " " + group + " " + request.getUri + " ----------------------------->")
    val r = Future.value(
      request.getUri match {
        case "/foo/good" => new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK))
        case _ => {
          println(" Token = " + request.headers().get("Auth-Token"))
          if (request.headers().get("Auth-Token")!= null && request.headers().get("Auth-Token").contains("LIVEKALE")){
            val resp = new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK))
            val cb: ChannelBuffer = ChannelBuffers.copiedBuffer("I am a Teapot from the MTP Service", Charset.defaultCharset())
            resp.setContent(cb)
            resp
          }else{
            new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.UNAUTHORIZED))
          }
        }
      }
    )
    println("<------------------------------ FooService " + " " + group + " " + request.getUri + " -----------------------------")
    r
  }

}