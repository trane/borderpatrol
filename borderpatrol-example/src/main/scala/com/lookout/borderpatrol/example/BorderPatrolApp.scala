package com.lookout.borderpatrol.example

import _root_.argonaut._, _root_.argonaut.Argonaut._
import com.lookout.borderpatrol.sessionx
import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.finagle._
import com.twitter.finagle.httpx.Cookie
import com.twitter.finagle.httpx.path.Root
import com.twitter.finagle.httpx.service.RoutingService
import com.twitter.io.Charsets
import com.twitter.server.TwitterServer
import com.twitter.util.{Future, Await, Base64StringEncoder}
import io.finch.response.{TurnIntoHttp, Unauthorized}
import io.finch.{Endpoint => _, _}
import io.finch.{HttpRequest, HttpResponse}
import io.finch.argonaut._
import io.finch.request._
import io.finch.request.items._
import io.finch.response._
import io.finch.route._

import scala.util.Success

object Main extends TwitterServer {
  import model._
  import reader._
  import endpoint._
  import sessionx._
  import io.finch.AnyOps

  def addCookie(response: HttpResponse): HttpResponse =
    tap(response) { rep =>
      rep.addCookie(new Cookie("border_session", SessionId.next))
    }

  val printer = new SimpleFilter[HttpRequest, HttpResponse] {
    def apply(req: HttpRequest, service: Service[HttpRequest, HttpResponse]): Future[HttpResponse] =
      service(req) map (rep => {println(s"$rep"); rep;})
  }

  val server = Httpx.serve(":8080", routes.toService)

}
