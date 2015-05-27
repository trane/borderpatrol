package com.lookout.borderpatrol.example

import _root_.argonaut._, _root_.argonaut.Argonaut._
import com.lookout.borderpatrol.sessionx
import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.finagle._
import com.twitter.finagle.httpx.Cookie
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

object Main extends App {
  val _ = Await.ready(Httpx.serve(":8080", BorderPatrolApp.backend))
}
object BorderPatrolApp extends TwitterServer {
  import model._
  import reader._
  import endpoint._
  import sessionx._
  import io.finch.AnyOps

  def gimmeCookie(id: SessionId): httpx.Cookie =
    tap(new Cookie("border_session", id.asBase64))(c => {
      // c.isSecure = true
    })

  val createIdOnLoginFilter = new SimpleFilter[HttpRequest, HttpResponse] {
    def apply(req: HttpRequest, service: Service[HttpRequest, HttpResponse]): Future[HttpResponse] =
      service(req) flatMap (rep => {
        rep.status match {
          case Unauthorized => Ok(Json("login please"))
          case _ => rep
        }
        val id = SessionId.next
        tap(rep)(rep.addCookie())
      })
  }
  val idFilter = new SimpleFilter[HttpRequest, HttpResponse] {
    def apply(req: HttpRequest, service: Service[HttpRequest, HttpResponse]): Future[HttpResponse] =
      for {
        id <- sessionId(req)
        rep <- service(req)
        _ <- rep.addCookie(gimmeCookie(id))
      } yield rep
  }

  val authorize = new Filter[HttpRequest, HttpResponse, AuthRequest, HttpResponse] {
    def apply(req: HttpRequest, service: Service[AuthRequest, HttpResponse]): Future[HttpResponse] =
      (for {
        sn <- session(req)
        up <- upstream(req)
        rep <- service(AuthRequest(sn.as[ApiKeySession].data(up), req))
      } yield tap(rep)(r => r.addCookie(gimmeCookie(sn.id)))) or Unauthorized().toFuture
  }

  val handleExceptions = new SimpleFilter[HttpRequest, HttpResponse] {
    def apply(req: HttpRequest, service: Service[HttpRequest, HttpResponse]): Future[HttpResponse] =
      service(req) handle {
        case NotValid(ParamItem(p), rule) => BadRequest(Json("error" := "param_not_valid", "param" := p, "rule" := rule))
        case RouteNotFound(r) => NotFound(Json("error" := "route_not_found", "route" := r))
        case _ => InternalServerError()
      }
  }

  val open: Service[HttpRequest, ToJson] = loginEP
  val api: Service[AuthRequest, ToJson] = businessEP

  val backend: Service[HttpRequest, HttpResponse] =
    (handleExceptions ! authorize ! (api ! TurnModelIntoJson ! TurnIntoHttp[Json]))

  def testExample(u: String, p: String) = {
    val req = httpx.Request(httpx.Method.Get, "/basic")
    val auth = Base64StringEncoder.encode(s"$u:$p".getBytes(Charsets.Utf8))
    req.authorization = s"Basic $auth"
    backend(req)
  }

}
