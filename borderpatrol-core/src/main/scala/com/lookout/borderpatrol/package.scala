package com.lookout

import java.io.FileReader
import java.net.{InetAddress, InetSocketAddress}

import com.lookout.borderpatrol.session._
import com.twitter.finagle.Service
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse, Http}
import com.twitter.finagle.loadbalancer.HeapBalancerFactory
import com.typesafe.config.ConfigFactory
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
      def apply(httpVersion: HttpVersion = HttpVersion.HTTP_1_1): HttpResponse =
        new DefaultHttpResponse(httpVersion, HttpResponseStatus.NOT_FOUND)
    }
    object OK {
      def apply(httpVersion: HttpVersion = HttpVersion.HTTP_1_1): HttpResponse =
        new DefaultHttpResponse(httpVersion, HttpResponseStatus.OK)
    }
  }

  /**
   * Build upstream clients from borderpatrol.conf. A map of the clients (where service name is the key)
   * gets passed to the UpstreamService, which dispatches requests based on the service name
   * @return
   */
  def getUpstreamClients: Map[String, Service[HttpRequest, HttpResponse]] = {
    val conf = ConfigFactory.parseReader(new FileReader("borderpatrol.conf"))
    val services = conf.getConfigList("services").toList
    case class ServiceConfiguration(name: String, friendlyName: String, hosts: String, rewriteRule: String) {}

    val clients = services map(s =>
      (s.getString("name"),
        ClientBuilder()
          .codec(Http())
          .hosts(s.getString("hosts"))
          .hostConnectionLimit(10)
          .loadBalancer(HeapBalancerFactory.toWeighted)
          .retries(2)
          .build()))
    clients.toMap
  }

  //Unsuccessful Response
  case class NeedsAuthResponse(httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse

  //Successful response
  case class Response(httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse

  implicit val upstreams = getUpstreamClients
}
