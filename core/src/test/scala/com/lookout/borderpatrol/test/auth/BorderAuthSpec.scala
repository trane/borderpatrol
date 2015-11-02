package com.lookout.borderpatrol.auth

import com.lookout.borderpatrol.sessionx.SessionStores.MemcachedStore
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.test._
import com.lookout.borderpatrol.{LoginManager, Manager, ServiceMatcher, ServiceIdentifier}
import com.lookout.borderpatrol.test.BorderPatrolSuite
import com.twitter.finagle.Service
import com.twitter.finagle.httpx._
import com.twitter.finagle.httpx.path.Path
import com.twitter.finagle.memcached
import com.twitter.finagle.memcached.GetResult
import com.twitter.io.Buf
import com.twitter.util.{Await, Future, Time}


class BorderAuthSpec extends BorderPatrolSuite  {
  import sessionx.helpers.{secretStore => store, _}

  //  Managers
  val keymasterIdManager = Manager("keymaster", Path("/identityProvider"), "localhost:8081")
  val keymasterAccessManager = Manager("keymaster", Path("/accessIssuer"), "localhost:8081")
  val checkpointLoginManager = LoginManager("checkpoint", Path("/check"), "localhost:8081", Path("/loginConfirm"),
    keymasterIdManager, keymasterAccessManager)

  // sids
  val one = ServiceIdentifier("one", "localhost:11", Path("/ent"), "enterprise", checkpointLoginManager)
  val serviceMatcher = ServiceMatcher(Set(one))
  val sessionStore = SessionStores.InMemoryStore

  // Request helper
  def req(subdomain: String = "nothing", path: String = "/"): Request =
    RequestBuilder().url(s"http://${subdomain + "."}example.com${path.toString}").buildGet()

  // Method to decode SessionData from the sessionId in Response
  def sessionDataFromResponse(resp: Response): Future[Request] =
    (for {
      sessionId <- SessionId.fromResponse(resp).toFuture
      sessionMaybe <- sessionStore.get[Request](sessionId)
    } yield sessionMaybe.fold[Identity[Request]](EmptyIdentity)(s => Id(s.data))).map(i => i match {
      case Id(req) => req
      case EmptyIdentity => null
    })

  //  Test Services
  def mkTestService[A](f: (A) => Future[Response]) : Service[A, Response] = new Service[A, Response] {
    def apply(request: A) = f(request)
  }
  val serviceFilterTestService = mkTestService[ServiceRequest] { req => Future.value(Response(Status.Ok)) }
  val sessionIdFilterTestService = mkTestService[SessionIdRequest] { req => Future.value(Response(Status.Ok)) }
  val identityFilterTestService = mkTestService[AccessIdRequest[Request]] { req => Future.value(Response(Status.Ok)) }

  //  Mock SessionStore client
  case object FailingMockClient extends memcached.MockClient {
    override def set(key: String, flags: Int, expiry: Time, value: Buf) : Future[Unit] = {
      Future.exception[Unit](new Exception("oopsie"))
    }
    override def getResult(keys: Iterable[String]): Future[GetResult] = {
      Future.exception(new Exception("oopsie"))
    }
  }

  behavior of "ServiceFilter"

  it should "succeed and return output of upstream Service if Request is destined to a known Service" in {
    // Execute
    val output = (new ServiceFilter(serviceMatcher) andThen serviceFilterTestService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "return NotFound Status if Request is destined to an unknown Service" in {
    // Execute
    val output = (new ServiceFilter(serviceMatcher) andThen serviceFilterTestService)(req("foo", "/bar"))

    // Validate
    Await.result(output).status should be (Status.NotFound)
  }

  behavior of "SessionIdFilter"

  it should "succeed and return output of upstream Service if ServiceRequest contains SessionId" in {
    val testService = mkTestService[SessionIdRequest] {
      request => {
        assert(request.req.req.path == "/ent")
        assert(request.req.serviceId == one)
        Future.value(Response(Status.Ok))
      }
    }

    //  Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    //  Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)

    //  Execute
    val output = (new SessionIdFilter(sessionStore) andThen testService)(ServiceRequest(request, one))

    //  Verify
    Await.result(output).status should be (Status.Ok)
  }

  it should "return redirect to login URI, if no SessionId present in the SessionIdRequest" in {

    // Create request
    val request = req("enterprise", "/ent")

    // Execute
    val output = (new SessionIdFilter(sessionStore) andThen sessionIdFilterTestService)(ServiceRequest(request, one))

    // Validate
    Await.result(output).status should be (Status.Found)
    Await.result(output).location should be equals(one.loginManager.path.toString)
    val sessionData = sessionDataFromResponse(Await.result(output))
    Await.result(sessionData).path should be equals(request.path)
  }

  it should "propagate the error Status code returned by the upstream Service" in {
    val testService = mkTestService[SessionIdRequest] { request => Future.value(Response(Status.NotFound))}

    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)

    // Execute
    val output = (new SessionIdFilter(sessionStore) andThen testService)(ServiceRequest(request, one))

    // Verify
    Await.result(output).status should be (Status.NotFound)
  }

  it should "propagate the Exception thrown by the upstream Service" in {
    val testService = mkTestService[SessionIdRequest] {
      request => Future.exception(new Exception("SessionIdFilter test failure"))
    }

    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)

    // Execute
    val output = (new SessionIdFilter(sessionStore) andThen testService)(ServiceRequest(request, one))

    // Verify
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("SessionIdFilter test failure")
  }

  it should "propagate the Exception thrown while storing the Session using SessionStore.update" in {
    // Mock sessionStore
    val mockSessionStore = MemcachedStore(FailingMockClient)

    // Create request
    val request = req("enterprise", "/ent")

    // Execute
    val output = (new SessionIdFilter(mockSessionStore) andThen sessionIdFilterTestService)(ServiceRequest(request, one))

    // Verify
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("oopsie")
  }

  behavior of "IdentityFilter"

  it should "succeed and return output of upstream Service, if Session is found for SessionId" in {
    val testService = mkTestService[AccessIdRequest[Int]] {
      request => {
        assert(request.id == Identity(999))
        Future.value(Response(Status.Ok)) }
    }

    //  Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    //  Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)
    val sessionData = 999
    sessionStore.update[Int](Session(sessionId, sessionData))

    //  Execute
    val output = (new IdentityFilter[Int](sessionStore) andThen testService)(SessionIdRequest(ServiceRequest(request, one), sessionId))

    //  Verify
    Await.result(output).status should be (Status.Ok)
  }

  it should "return a redirect to login UTI, if it fails Session lookup using SessionId" in {

    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)

    // Execute
    val output = (new IdentityFilter[Request](sessionStore) andThen identityFilterTestService)(SessionIdRequest(ServiceRequest(request, one), sessionId))

    // Verify
    Await.result(output).status should be (Status.Found)
    Await.result(output).location should be equals(one.loginManager.path.toString)
    val returnedSessionId = SessionId.fromResponse(Await.result(output)).toFuture
    Await.result(returnedSessionId) should not equals(sessionId)
    val sessionData = sessionDataFromResponse(Await.result(output))
    Await.result(sessionData).path should be equals(request.path)
  }

  it should "propagate the exception thrown by SessionStore.get operation" in {
    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)

    // Mock sessionStore
    val mockSessionStore = MemcachedStore(FailingMockClient)

    // Execute
    val output = (new IdentityFilter[Request](mockSessionStore) andThen identityFilterTestService)(SessionIdRequest(ServiceRequest(request, one), sessionId))

    // Verify
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("oopsie")
  }

  it should "propagate the Exception thrown by SessionStore.update operation" in {
    //  Mock SessionStore client
    case object FailingUpdateMockClient extends memcached.MockClient {
      override def set(key: String, flags: Int, expiry: Time, value: Buf) : Future[Unit] = {
        Future.exception[Unit](new Exception("whoopsie"))
      }
    }

    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Mock sessionStore
    val mockSessionStore = MemcachedStore(FailingUpdateMockClient)

    // Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)

    // Execute
    val output = (new IdentityFilter[Request](mockSessionStore) andThen identityFilterTestService)(SessionIdRequest(ServiceRequest(request, one), sessionId))

    // Verify
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("whoopsie")
  }

  behavior of "ExceptionFilter"

  it should "succeed and act as a passthru for the valid Response returned by Service" in {
    val testService = mkTestService[Request] { req => Future.value(Response(Status.Ok)) }

    // Execute
    val output = (new ExceptionFilter andThen testService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "succeed and convert the AccessDenied exception into error Response" in {
    val testService = mkTestService[Request] { req =>
      Future.exception(AccessDenied(Status.NotAcceptable, "No access allowed to service"))
    }

    // Execute
    val output = (new ExceptionFilter andThen testService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.NotAcceptable)
    Await.result(output).contentString should be ("AccessDenied: No access allowed to service")
  }

  it should "succeed and convert the SessionStoreError exception into error Response" in {
    val testService = mkTestService[Request] { req =>
      Future.exception(new SessionStoreError("update failed"))
    }

    // Execute
    val output = (new ExceptionFilter andThen testService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.InternalServerError)
    Await.result(output).contentString should be ("An error occurred interacting with the session store: update failed")
  }

  it should "succeed and convert the AccessIssuerError exception into error Response" in {
    val testService = mkTestService[Request] { req =>
      Future.exception(AccessIssuerError(Status.NotAcceptable, "Some access issuer error"))
    }

    // Execute
    val output = (new ExceptionFilter andThen testService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.NotAcceptable)
    Await.result(output).contentString should be ("Some access issuer error")
  }

  it should "succeed and convert the IdentityProviderError exception into error Response" in {
    val testService = mkTestService[Request] { req =>
      Future.exception(IdentityProviderError(Status.NotAcceptable, "Some identity provider error"))
    }

    // Execute
    val output = (new ExceptionFilter andThen testService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.NotAcceptable)
    Await.result(output).contentString should be ("Some identity provider error")
  }

  it should "succeed and convert the Runtime exception into error Response" in {
    val testService = mkTestService[Request] { req =>
      Future.exception(new RuntimeException("some weird exception"))
    }

    // Execute
    val output = (new ExceptionFilter andThen testService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.InternalServerError)
    Await.result(output).contentString should be ("some weird exception")
  }
}