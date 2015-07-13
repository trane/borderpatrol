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

object Main extends App {
  val _ = Await.ready(Httpx.serve(":8080", BorderPatrolApp.authBackend))
}
object BorderPatrolApp extends TwitterServer {
  import model._
  import reader._
  import endpoint._
  import sessionx._
  import io.finch.AnyOps

  def addCookie(response: HttpResponse): HttpResponse =
    tap(response) { rep =>
      rep.addCookie(new Cookie("border_session", SessionId.next))
    }

  val createIdOnLoginFilter = new SimpleFilter[HttpRequest, HttpResponse] {
    def apply(req: HttpRequest, service: Service[HttpRequest, HttpResponse]): Future[HttpResponse] =
      service(req) map (rep => (
        rep.status match {
          case httpx.Status.Unauthorized => Ok(Json("error" := "login please"))
          case httpx.Status.Ok => addCookie(rep)
          case _ => rep
        }
      ))
  }

  val authorize = new Filter[HttpRequest, HttpResponse, AuthRequest, HttpResponse] {
    def apply(req: HttpRequest, service: Service[AuthRequest, HttpResponse]): Future[HttpResponse] =
      for {
        id <- requireSessionId(req)
        sn <- session(id)(req)
        up <- upstream(req)
        rep <- service(AuthRequest(sn.as[ApiKeySession].data(up), req))
      } yield rep
  }

  val loginPath = "/login"
  val handleExceptions = new SimpleFilter[HttpRequest, HttpResponse] {
    def apply(req: HttpRequest, service: Service[HttpRequest, HttpResponse]): Future[HttpResponse] =
      service(req) handle {
        case NotPresent(e) | InvalidSessionId(ee) => SeeOther.withHeaders(("Location", loginPath))()
        case NotValid(ParamItem(p), rule) => BadRequest(Json("error" := "param_not_valid", "param" := p, "rule" := rule))
        case RouteNotFound(r) => NotFound(Json("error" := "route_not_found", "route" := r))
        case e => InternalServerError(Json("error" := s"$e"))
      }
  }

  val printer = new SimpleFilter[HttpRequest, HttpResponse] {
    def apply(req: HttpRequest, service: Service[HttpRequest, HttpResponse]): Future[HttpResponse] =
      service(req) map (rep => {println(s"$rep"); rep;})
  }

  /**
  val authBackend: Service[HttpRequest, HttpResponse] =
    handleExceptions ! (authorize ! (api ! TurnModelIntoJson ! TurnIntoHttp[Json]))
  val loginBackend: Service[HttpRequest, HttpResponse] =
    handleExceptions ! (open ! TurnModelIntoJson ! TurnIntoHttp[Json])
  */

  val server = Httpx.serve(":8080", routes.toService)

  def testAuth(req: HttpRequest = httpx.Request(httpx.Method.Get, "/service1/blah")) = {
    //req.addCookie(new Cookie("border_session", SessionId.next))
    //authBackend(req)
  }

}
