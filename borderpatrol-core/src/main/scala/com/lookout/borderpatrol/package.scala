package com.lookout

import java.net.{InetAddress, InetSocketAddress}

import com.lookout.borderpatrol.session._
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse}
import org.jboss.netty.handler.codec.http._

import scala.collection.JavaConversions._

package object borderpatrol {

  case class RoutedRequest(httpRequest: HttpRequest, service: String, session: Session) extends FinagleRequest {
    override val remoteSocketAddress: InetSocketAddress = new InetSocketAddress(InetAddress.getLoopbackAddress, 0) //TODO: This is wrong

    def +=(otherSession: Session): RoutedRequest =
      copy(session = otherSession)

    def +(token: Token): RoutedRequest =
      this += session.copy(tokens = session.tokens += token)

    def ++(tokens: ServiceTokens): RoutedRequest =
      this += session.copy(tokens = session.tokens ++= tokens)

    def ++(tokens: Tokens): RoutedRequest =
      this += session.copy(tokens = session.tokens ++= tokens)

    def clearTokens: RoutedRequest =
      this += session.copy(tokens = Tokens.empty)

    def serviceToken: Option[ServiceToken] =
      session.tokens.service(service)

    def toHttpRequest: HttpRequest = {
      addAuthHeaders
      addBorderHeaders
      httpRequest
    }

    /* TODO: make this more functional */
    def addAuthHeaders: Unit =
      serviceToken.foreach(t => httpRequest.headers.add("Auth-Token", t.value))

    /* TODO: make this more functional */
    def addBorderHeaders: Unit =
      httpRequest.headers.add("Via", "Border Patrol")

    def useOriginalUri: Unit =
      httpRequest.setUri(session.originalRequest.getUri)

    def useOriginalMethod: Unit =
      httpRequest.setMethod(session.originalRequest.getMethod)

    def borderCookie: Option[String] =
      cookies.getValue(Session.cookieName)
  }

  object RoutedRequest {
    val decoder = new CookieDecoder()

    def apply(request: HttpRequest, name: String): RoutedRequest = {
      val session = borderCookieValue(request).fold(Session.newSession(request))(Session(_, request))
      RoutedRequest(request, name, session)
    }

    def borderCookieValue(request: HttpRequest): Option[String] = {
      val cookie: Option[Cookie] = (for {
        header <- request.headers.getAll(HttpHeaders.Names.COOKIE).toList
        cookie <- decoder.decode(header).toSet
        if cookie.getName == Session.cookieName
      } yield cookie).headOption
      cookie map (_.getValue)
    }
  }

  object Responses {
    object NotFound {
      def apply(httpVersion: HttpVersion = HttpVersion.HTTP_1_1): Response =
        Response(new DefaultHttpResponse(httpVersion, HttpResponseStatus.NOT_FOUND))
    }
    object OK {
      def apply(httpVersion: HttpVersion = HttpVersion.HTTP_1_1): Response =
        Response(new DefaultHttpResponse(httpVersion, HttpResponseStatus.OK))
    }
  }

  //Unsuccessful Response
  case class NeedsAuthResponse(httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse

  //Successful response
  case class Response(httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse

}
