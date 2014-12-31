package com.lookout

import java.net.{InetAddress, InetSocketAddress}

import com.lookout.borderpatrol.session._
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse, CookieMap, RequestProxy, HttpMuxer, Http}
import com.twitter.finagle.{Service => FinagleService}
import com.twitter.util.Future

import org.jboss.netty.handler.codec.http._

package object borderpatrol {

  sealed trait RoutedRequest extends FinagleRequest {
    val service: String
    val session: Session
    override val remoteSocketAddress: InetSocketAddress = new InetSocketAddress(InetAddress.getLoopbackAddress, 0) //TODO: This is wrong

    def +=(otherSession: Session): RoutedRequest

    def +(token: MasterToken): RoutedRequest =
      this += Session(session.id, session.originalRequest, session.tokens += token)

    def ++(tokens: ServiceTokens): RoutedRequest =
      this += Session(session.id, session.originalRequest, session.tokens ++= tokens)

    def ++(tokens: Tokens): RoutedRequest =
      this += Session(session.id, session.originalRequest, session.tokens ++= tokens)

    def clearTokens: RoutedRequest =
      this += Session(session.id, session.originalRequest, Tokens(EmptyToken, EmptyServiceTokens))

    def serviceToken: Option[ServiceToken] =
      session.tokens.service(service)

    def toHttpRequest: HttpRequest = {
      addAuthHeaders
      addBorderHeaders
      useOriginalUri
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
      cookies.getValue(SecureSession.cookieName)
  }

  object RoutedRequest {
    def apply(request: HttpRequest, name: String): RoutedRequest =
      RoutedRequestInferSession(request, name)
    def apply(request: HttpRequest, name: String, session: Session): RoutedRequest =
      RoutedRequestWithSession(request, name, session)
  }

  case class RoutedRequestInferSession(httpRequest: HttpRequest, service: String) extends RoutedRequest {
    val session = Session(this)

    def +=(otherSession: Session): RoutedRequest =
      RoutedRequest(httpRequest, service, otherSession)
  }
  /**
   * The base Request type in Border Patrol
   * It contains the session info and service information needed to do routing and rehydrate previous requests
   * @param httpRequest Underlying netty http request
   * @param service The service name
   * @param session A session, where information for this and future requests lives
   */
  case class RoutedRequestWithSession(httpRequest: HttpRequest, service: String, session: Session) extends RoutedRequest {
    def +=(otherSession: Session): RoutedRequest =
      copy(httpRequest, service, otherSession)
  }

  //Unsuccessful Response
  case class NeedsAuthResponse(httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse

  //Successful response
  case class Response(httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse

}
