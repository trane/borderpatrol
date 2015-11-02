package com.lookout.borderpatrol.example

import com.twitter.finagle.httpx.{Response, Request}
import com.twitter.finagle.{Service, Httpx}
import com.twitter.server.TwitterServer
import com.twitter.util.Await

object BorderPatrolApp extends TwitterServer with Config {
  import service._
  import MockService._
  import Config._

  premain {
    implicit val serverConfig = readServerConfig(configFile())

    val server1 = Httpx.serve(":8080", MainServiceChain)
    val server2 = Httpx.serve(":8081", getMockRoutingService)
    Await.all(server1, server2)
  }
}
