package com.lookout.borderpatrol

import java.net.{InetAddress, InetSocketAddress}

import com.lookout.borderpatrol.session._
import com.twitter.finagle.http.{Http, Request => FinagleRequest, Response => FinagleResponse}
import com.twitter.finagle.http.{Request => FinagleRequest, Response => FinagleResponse, HttpMuxer, Http}
import com.twitter.finagle.http.{RichHttp, Response, Request}
import com.twitter.finagle.{Filter, _}
import com.twitter.server.TwitterServer
import org.jboss.netty.handler.codec.http._
import com.twitter.util.Future

object BorderPatrolApp extends TwitterServer {

  //A Request with Routing and Session Information
  case class RoutedRequest(httpRequest: HttpRequest, service: String, session: Session) extends FinagleRequest {
    override val remoteSocketAddress: InetSocketAddress = new InetSocketAddress(InetAddress.getLoopbackAddress, 0) //TODO: This is wrong

    def +=(otherSession: Session): RoutedRequest =
      copy(httpRequest, service, otherSession)

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
      httpRequest
    }

    /* TODO: make this more functional */
    def addAuthHeaders: Unit =
      serviceToken.foreach(t => httpRequest.headers.add("Auth-Token", t.value))

    /* TODO: make this more functional */
    def addBorderHeaders: Unit =
      httpRequest.headers.add("Via", "Border Patrol")
  }

  //Unsuccessful Response
  case class NeedsAuthResponse(httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse

  //Successful response
  case class Response(httpResponse: HttpResponse) extends FinagleResponse //with BorderPatrolResponse

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
                "flexd": "LIVEKALE",
                "mtp": "DEADLAKE"
            }
        }
      """
  }

  val sessionFilter = new SessionFilter
  var authService = new AuthService(new TokenService, new LoginService)
  val upstreamService = new UpstreamService(authService)
  val upstreamFilter = new UpstreamFilter(authService)
  val routingFilter = new RoutingFilter
  val loginFilter = new LoginFilter

  val orchestratorService = routingFilter andThen sessionFilter andThen upstreamFilter andThen upstreamService


  def main() {
    val server: Server = ServerBuilder()
      .codec(Http())
      .bindTo(new InetSocketAddress(8080))
      .name("BorderPatrol-1")
      .build(orchestratorService)

    HttpMuxer.addHandler("/mtp", new MtpService)
    HttpMuxer.addHandler("/mtp/", new MtpService)

    val server2: Server = ServerBuilder()
      .codec(Http())
      .bindTo(new InetSocketAddress(8081))
      .name("BorderPatrol-2")
      .build(HttpMuxer)
  }
}
