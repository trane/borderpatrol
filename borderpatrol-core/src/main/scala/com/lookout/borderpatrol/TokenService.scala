package com.lookout.borderpatrol

import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._

class TokenService extends Service[HttpRequest, FinagleResponse] {
  def apply(request: HttpRequest) =
    Future.value(respond(new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK))))

  def respond(response: Response): Response = {
    response.setContentTypeJson()
    response.setContentString(mockJsonResponse)
    response
  }

  def mockJsonResponse: String =
    """
       {
            "auth_service": "DEADBEEF",
            "service_tokens": {
                "foo": "LIVEKALE",
                "bar": "WOUNDEDCAKE",
                "baz": "HOTCAFE"
            }
        }
    """
}

