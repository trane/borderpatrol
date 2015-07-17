package com.lookout.borderpatrol.example

import com.lookout.borderpatrol.sessionx
import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.finagle._
import com.twitter.server.TwitterServer
import com.twitter.util.{Future}
import io.finch.{Endpoint => _, _}
import io.finch.{HttpRequest, HttpResponse}

object Main extends TwitterServer {
  import model._
  import reader._
  import endpoint._
  import sessionx._
  import io.finch.AnyOps

  val printer = new SimpleFilter[HttpRequest, HttpResponse] {
    def apply(req: HttpRequest, service: Service[HttpRequest, HttpResponse]): Future[HttpResponse] =
      for {
        sesReq <- Session(req)
        res <- service(req)
        if res.status == httpx.Status.Unauthorized
        cookie <- generateCookie(sesReq.id)
      } yield tap(res)(_.addCookie(cookie))
  }

  val server = Httpx.serve(":8080", routes.toService)

}
