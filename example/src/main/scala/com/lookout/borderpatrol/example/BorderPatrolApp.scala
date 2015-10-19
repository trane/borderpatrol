package com.lookout.borderpatrol.example

import com.twitter.finagle.Httpx
import com.twitter.util.Await

object BorderPatrolApp extends App {
  import service._

  val server1 = Httpx.serve(":8080", routingService1)
  val server2 = Httpx.serve(":8081", routingService2)
  Await.all(server2, server1)
}
