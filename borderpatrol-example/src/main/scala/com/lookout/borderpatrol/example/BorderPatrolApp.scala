package com.lookout.borderpatrol.example

import com.twitter.finagle.Httpx
import com.twitter.util.Await

object Main extends App {
  import endpoint._

  val server = Httpx.serve(":8080", routes.toService)
  Await.ready(server)
}
