package com.lookout.borderpatrol.example

import com.lookout.borderpatrol.sessionx
import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.finagle._
import com.twitter.finagle.httpx.{Response, Request}
import com.twitter.server.TwitterServer
import com.twitter.util.{Await, Future}
import io.finch.{Endpoint => _, _}

object Main extends TwitterServer {
  import reader._
  import sessionx._
  import io.finch.request._
  import io.finch.route._

  val printer = new Filter[Request, Response, Request, Response] {
    def apply(req: Request, service: Service[Request, Response]): Future[Response] =
      for {
        sesReq <- Session(req)
        res <- service(req)
        if res.status == httpx.Status.Unauthorized
      } yield tap(res)(_.addCookie(generateCookie(sesReq.id)))
  }

  val server = Httpx.serve("localhost:8080", (Get / "service" /> printer).toService)
  Await.ready(server)
}
