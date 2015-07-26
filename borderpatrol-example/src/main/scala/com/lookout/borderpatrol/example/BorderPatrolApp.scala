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
  import endpoint._
  import io.finch.request._
  import io.finch.route._


  val server = Httpx.serve(":8080", routes.toService)
  Await.ready(server)
}
