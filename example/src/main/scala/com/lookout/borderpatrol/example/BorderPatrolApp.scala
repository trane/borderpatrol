package com.lookout.borderpatrol.example

import com.lookout.borderpatrol.{ServerConfig, Config}
import com.twitter.finagle.Httpx
import com.twitter.server.TwitterServer
import com.twitter.util.Await

object BorderPatrolApp extends TwitterServer with Config {
  import service._
  import ServerConfig._

  premain {
    implicit val serverConfig = ServerConfig(secretStore(), sessionStore(), serviceIds())

    val server1 = Httpx.serve(":8080", getRoutingService)
    val server2 = Httpx.serve(":8081", getMockRoutingService)
    Await.all(server1, server2)
  }
}
