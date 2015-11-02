package com.lookout.borderpatrol.test

import com.lookout.borderpatrol._
import com.lookout.borderpatrol.Binder._
import com.lookout.borderpatrol.{LoginManager, Manager, ServiceIdentifier, ServiceMatcher}
import com.twitter.finagle.Service
import com.twitter.finagle.httpx.{Status, Response, RequestBuilder, Request}
import com.twitter.finagle.httpx.path.Path
import com.twitter.util.{Await, Future}

class BinderSpec extends BorderPatrolSuite {
  val keymasterIdManager = Manager("keymaster", Path("/identityProvider"), "localhost:8081")

  // Request helper
  def req(path: String): Request =
    Request(s"http://localhost${path.toString}")

  def mkTestService[A](f: (A) => Future[Response]) : Service[A, Response] = new Service[A, Response] {
    def apply(request: A) = f(request)
  }

  behavior of "ManagerBinder"

  it should "successfully connect to server and returns the response" in {
    val managerBinder = ManagerBinder()
    val server = com.twitter.finagle.Httpx.serve(
      "localhost:8081", mkTestService[Request]{_ => Response(Status.NotAcceptable).toFuture })
    try {
      val bindReq = BindRequest[Manager](keymasterIdManager, req(keymasterIdManager.path.toString))
      val output = managerBinder(bindReq)
      Await.result(output).status should be(Status.NotAcceptable)
      /* Make sure client is cached in the cache */
      managerBinder.get(keymasterIdManager.name) should not be None
    } finally {
      server.close()
    }
  }
}