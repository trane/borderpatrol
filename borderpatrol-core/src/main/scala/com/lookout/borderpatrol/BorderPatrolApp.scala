package com.lookout.borderpatrol

import com.lookout.borderpatrol.routing._
import com.lookout.borderpatrol.auth._
import com.lookout.borderpatrol.sessions._
import com.twitter.finagle.{Filter, _}
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.server.TwitterServer
import com.twitter.util._
import org.jboss.netty.handler.codec.http._

object BorderPatrolApp extends TwitterServer {

  val sessionStore = new SessionStore

  val sessionIDFilter = new SessionIDFilter(sessionStore)
  val routingFilter = new RoutingFilter
  val sessionStoreFilter = new SessionStoreFilter(sessionStore)
  val authFilter = new AuthFilter
  val authService = new AuthService

  val orchestratorFilter: Filter[HttpRequest, HttpResponse, HttpRequest, HttpResponse] =
    sessionIDFilter andThen routingFilter andThen sessionStoreFilter andThen authFilter

  val service = new Service[HttpRequest, HttpResponse] {
    def apply(request: HttpRequest) = {
      log.info("Service: Received a request at " + Time.now + ". Calling upstream =" + request.getUri())

      val newUri = request.getUri.split("/").toSeq match {
        case Seq() => "/"
        case x => x.tail.tail.mkString("/", "/", "")
      }

      log.info("new uri: " + newUri)
      request.setUri(newUri)
      //call upstream
      val client: Service[HttpRequest, HttpResponse] = ClientBuilder()
        .codec(com.twitter.finagle.http.Http())
        .hosts(request.getHeader("HOST")) // If >1 host, client does simple load-balancing
        .hostConnectionLimit(1)
        .build()

      val clientWithTokenFilter = new TokenFilter(sessionStore) andThen client

      clientWithTokenFilter(request)
    }
  }

  val orchestratorService: Service[HttpRequest, HttpResponse] =
    orchestratorFilter andThen service

  def main() {
   val server = Http.serve(":8080", orchestratorService)
    onExit {
      server.close()
    }
   Await.ready(server)
  }
}