package com.lookout.borderpatrol.auth

import org.jboss.netty.buffer._
import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse, HttpResponseStatus}
import com.twitter.finagle.{SimpleFilter, Service}
import org.jboss.netty.buffer.ChannelBuffers.copiedBuffer
import org.jboss.netty.util.CharsetUtil.UTF_8

/**
 * Transforms an upstream responses 401 UNAUTHORIZED into a 301 Redirect to Checkpoint
 *
 * Created by akuhnhausen on 6/11/14.
 */
class AuthFilter extends SimpleFilter[HttpRequest, HttpResponse] {
  def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]) = {

    // When the upstream have us a 401, transform it to a 302 and return the auth URL.
    service(request).onSuccess(resp => {
      if(resp.getStatus == HttpResponseStatus.UNAUTHORIZED) {
        resp.setStatus(HttpResponseStatus.MOVED_PERMANENTLY)
        // TODO: this URL should come from the RoutingService
        resp.setContent(copiedBuffer("", UTF_8))
        resp.setHeader("Content-Length", "0")
        resp.setHeader("Location", "/a/auth")
      }
    })
  }
}