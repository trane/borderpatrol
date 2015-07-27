package com.lookout.borderpatrol.example

import com.lookout.borderpatrol.sessionx
import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.finagle.{Httpx, Service}
import com.twitter.finagle.httpx.{Response, Request}
import com.twitter.server.TwitterServer
import com.twitter.util.{Await, Future}


object Main extends App {
  import reader._
  import sessionx._
  import io.finch.request._
  import io.finch.route._


  val serviceEndpoints: Service[Request, Response] = (
      (* / "service1" /> service.service1) :+:
      (* / "service2" /> service.service2) :+:
      (Post / "login" /> service.LoginService) :+:
      (Get / "login" /> service.LoginService.ok)
      ).toService

  val server = Httpx.serve(":8080", serviceEndpoints)
  Await.ready(server)
}
