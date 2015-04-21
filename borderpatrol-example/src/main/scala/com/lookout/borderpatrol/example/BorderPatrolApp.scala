package com.lookout.borderpatrol.example

import com.lookout.borderpatrol.util.Combinators.tap
import com.lookout.borderpatrol.auth._
import com.lookout.borderpatrol._
import com.twitter.finagle.httpx.path._
import com.twitter.finagle.httpx.service.RoutingService

import com.twitter.finagle.{Service, httpx}
import com.twitter.io.Charsets
import com.twitter.server.TwitterServer
import com.twitter.util.Base64StringEncoder

object BorderPatrolApp extends TwitterServer {

  object Basic {
    import BasicAuth._

    val auth = BasicAuthFilter

    val service = new Service[BorderRequest[Basic], httpx.Response] {
      def apply(request: BorderRequest[Basic]) = {
        val session = Session(request.request)

        val cred = request.authInfo.info.credential.get
        val body = s"You have authenticated with $cred"
        println(body)
        tap(httpx.Response(httpx.Status.Ok))(r => r.contentString = body).toFuture
      }
    }
  }

  object OAuth {
    val auth: BorderFilter[OAuth2] = ???
    val service: Service[BorderRequest[OAuth2], httpx.Response] = ???
  }

  val backend = RoutingService.byPathObject {
    case Root / "basic" => Basic.auth andThen Basic.service
    case Root / "oauth2" => OAuth.auth andThen OAuth.service
  }

  def testExample(u: String, p: String) = {
    val req = httpx.Request(httpx.Method.Get, "/basic")
    val auth = Base64StringEncoder.encode(s"$u:$p".getBytes(Charsets.Utf8))
    req.authorization = s"Basic $auth"
    backend(req)
  }

}
