package com.lookout.borderpatrol.test

import java.net.URL

import com.lookout.borderpatrol._
import com.lookout.borderpatrol.Binder._
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Status, Response, Request}
import com.twitter.finagle.http.path.Path
import com.twitter.util.{Await, Future}

class BinderSpec extends BorderPatrolSuite {
  val urls = Set(new URL("http://localhost:5679"))

  override def afterEach(): Unit = {
    BinderBase.clear
  }

  //  Managers
  val keymasterIdManager = Manager("keymaster", Path("/identityProvider"), urls)
  val keymasterAccessManager = Manager("keymaster", Path("/accessIssuer"), urls)
  val internalProtoManager = InternalAuthProtoManager(Path("/loginConfirm"), Path("/check"), urls)
  val checkpointLoginManager = LoginManager("checkpoint", keymasterIdManager, keymasterAccessManager,
    internalProtoManager)

  // sids
  val one = ServiceIdentifier("one", urls, Path("/ent"), None)
  val cust = CustomerIdentifier("enterprise", one, checkpointLoginManager)

  // Request helper
  def req(path: String): Request =
    Request(s"http://localhost${path.toString}")

  def mkTestService[A](f: (A) => Future[Response]) : Service[A, Response] = new Service[A, Response] {
    def apply(request: A) = f(request)
  }

  behavior of "ManagerBinder"

  it should "successfully connect to server and returns the response" in {
    val server = com.twitter.finagle.Http.serve(
      "localhost:5679", mkTestService[Request]{_ => Response(Status.NotAcceptable).toFuture })
    try {
      val bindReq = BindRequest[Manager](keymasterIdManager, req(keymasterIdManager.path.toString))
      val output = ManagerBinder(bindReq)
      Await.result(output).status should be(Status.NotAcceptable)
      /* Make sure client is cached in the cache */
      BinderBase.get(keymasterIdManager.name) should not be None
    } finally {
      server.close()
    }
  }

  behavior of "LoginManagerBinder"

  it should "successfully connect to server and returns the response" in {
    val server = com.twitter.finagle.Http.serve(
      "localhost:5679", mkTestService[Request]{_ => Response(Status.NotAcceptable).toFuture })
    try {
      val bindReq = BindRequest[LoginManager](checkpointLoginManager,
        req(checkpointLoginManager.protoManager.redirectLocation(None)))
      val output = LoginManagerBinder(bindReq)
      Await.result(output).status should be(Status.NotAcceptable)
      /* Make sure client is cached in the cache */
      BinderBase.get(checkpointLoginManager.name) should not be None
    } finally {
      server.close()
    }
  }

  behavior of "ServiceIdentifierBinder"

  it should "successfully connect to server and returns the response" in {
    val server = com.twitter.finagle.Http.serve(
      "localhost:5679", mkTestService[Request]{_ => Response(Status.NotAcceptable).toFuture })
    try {
      val bindReq = BindRequest[ServiceIdentifier](one, req(one.path.toString))
      val output = ServiceIdentifierBinder(bindReq)
      Await.result(output).status should be(Status.NotAcceptable)
      /* Make sure client is cached in the cache */
      BinderBase.get(one.name) should not be None
    } finally {
      server.close()
    }
  }
}
