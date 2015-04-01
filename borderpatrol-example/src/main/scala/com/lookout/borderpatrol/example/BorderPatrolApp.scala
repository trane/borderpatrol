package com.lookout.borderpatrol.example

import java.net.InetSocketAddress

import com.lookout.borderpatrol._
import com.twitter.finagle.Http
import com.twitter.finagle.httpx.service.RoutingService
import com.twitter.finagle.httpx.{Request, Response, HttpMuxer}
import com.twitter.server.TwitterServer
import com.twitter.util.Await

object BorderPatrolApp extends TwitterServer {

}
