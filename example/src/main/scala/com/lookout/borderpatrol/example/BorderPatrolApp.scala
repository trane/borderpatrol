package com.lookout.borderpatrol.example

import com.lookout.borderpatrol.server._
import com.twitter.finagle.Httpx
import com.twitter.server.TwitterServer
import com.twitter.util.Await

object BorderPatrolApp extends TwitterServer with Config {
  import services._
  import MockService._
  import Config._

  premain {
    implicit val serverConfig = readServerConfig(configFile())

    val server1 = Httpx.serve(":8080", MainServiceChain)
    val server2 = Httpx.serve(":8081", getMockRoutingService)
    Await.all(server1, server2)
  }
}
