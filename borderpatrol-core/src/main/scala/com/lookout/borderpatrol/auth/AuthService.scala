package com.lookout.borderpatrol.auth

import com.twitter.finagle._
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http.{HttpResponseStatus, DefaultHttpResponse, HttpRequest, HttpResponse}

/**
 * Invokes the Authentication service to turn a master session token into a token for a specific service.
 *
 * Created by akuhnhausen on 6/11/14.
 */
class AuthService extends Service[HttpRequest, HttpResponse] {
  def apply(request: HttpRequest) = {
    val response = new DefaultHttpResponse(request.getProtocolVersion, HttpResponseStatus.OK)
    Future.value(response)
  }
}
