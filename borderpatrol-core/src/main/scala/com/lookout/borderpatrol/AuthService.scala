package com.lookout.borderpatrol

import com.lookout.borderpatrol.BorderPatrolApp.Response
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._

/**
 * Created by wkimeria on 12/11/14.
 */
class AuthService extends Service[HttpRequest, FinagleResponse] {
  def apply(request: HttpRequest) = {
    println("----------------------------- AuthService------------------------------>")
    var tokens =
      """
       {
            "auth_service": "DEADBEEF",
            "service_tokens": {
                "flexd": "LIVEKALE",
                "mtp": "DEADLAKE"
            }
        }
      """
    val result: FinagleResponse = new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK))
    result.setContentString(tokens)
    result.setContentTypeJson()
    val r = Future.value(result)
    println("<----------------------------- AuthService------------------------------")
    r
  }
}
