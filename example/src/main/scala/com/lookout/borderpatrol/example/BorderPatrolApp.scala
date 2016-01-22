package com.lookout.borderpatrol.example

import com.lookout.borderpatrol.server._
import com.twitter.finagle.Http
import com.twitter.server.TwitterServer
import com.twitter.util.Await

object BorderPatrolApp extends TwitterServer with Config {
  import service._
  import MockService._
  import Config._

  premain {
    implicit val serverConfig = readServerConfig(configFile())
    implicit val bpStatsReceiver = statsReceiver

    // Create a StatsD exporter
    val statsdReporter = new StatsdExporter(serverConfig.statsdExporterConfig)

    // Create a server
    val server1 = Http.serve(":8080", MainServiceChain)
    val server2 = Http.serve(":8081", getMockRoutingService)
    Await.all(server1, server2)
  }
}
