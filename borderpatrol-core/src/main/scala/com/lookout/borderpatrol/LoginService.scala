package com.lookout.borderpatrol

import com.twitter.finagle.Service
import com.twitter.finagle.http.{Response => FinagleResponse}
import com.twitter.io.Charsets
import com.twitter.util.Future
import org.jboss.netty.buffer.ChannelBuffers.copiedBuffer
import org.jboss.netty.handler.codec.http.{DefaultHttpResponse, HttpMethod, HttpResponseStatus, HttpVersion}

/**
 * Handles call to checkpoint
 */
class LoginService extends Service[RoutedRequest, FinagleResponse] {
  def apply(request: RoutedRequest) = {
    println("----------------------------- LoginService------------------------------>")
    val r = if (request.method == HttpMethod.POST)
              Future.value(respondWithTokens(new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK))))
            else
              Future.value(respondWithWebpage(new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK))))
    println("<----------------------------- LoginService------------------------------")
    r
  }

  def respondWithWebpage(response: Response): Response = {
    response.setContent(copiedBuffer(mockLoginPageResponse, Charsets.Utf8))
    response
  }

  def respondWithTokens(response: Response): Response = {
    response.setContentTypeJson()
    response.setContent(copiedBuffer(mockJsonResponse, Charsets.Utf8))
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

  def mockLoginPageResponse: String =
    """
       <html>
        <body>
          Login Page!
          <form action="/a/login" method="post">
            <label for="username">Username</label>
            <input name="username" type="text" value="user@example.com" />
            <br />
            <label for="password">Password</label>
            <input name="password" type="password" value="password" />
            <br />
            <input name="login" type="submit" value="login" />
          </form>
        </body>
       </html>
    """
}
