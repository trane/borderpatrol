package com.lookout.borderpatrol.example

import com.lookout.borderpatrol.util.Combinators.tap
import com.lookout.borderpatrol.auth._
import BasicAuth._
import com.lookout.borderpatrol._
import com.twitter.finagle.httpx.path._
import com.twitter.finagle.httpx.service.RoutingService

import com.twitter.finagle.{Service, httpx}
import com.twitter.io.Charsets
import com.twitter.server.TwitterServer
import com.twitter.util.{Base64StringEncoder, Await}

object BorderPatrolApp extends TwitterServer {

  val basicAuth = BasicAuthFilter

  val basicAuthService = new Service[BorderRequest[Basic], httpx.Response] {
    def apply(request: BorderRequest[Basic]) = {
      val cred = request.authInfo.info.credential.get
      val body = s"You have authenticated with $cred"
      println(body)
      tap(httpx.Response(httpx.Status.Ok))(r => r.contentString = body).toFuture
    }
  }

  val backend = RoutingService.byPathObject {
    case Root / "hello" => basicAuth andThen basicAuthService
  }

  def testExample(u: String, p: String) = {
    val req = httpx.Request(httpx.Method.Get, "/hello")
    val auth = Base64StringEncoder.encode(s"$u:$p".getBytes(Charsets.Utf8))
    req.authorization = s"Basic $auth"
    backend(req)
  }

}
