package com.lookout.borderpatrol.auth

import com.lookout.borderpatrol.sessionx.SessionStores.MemcachedStore
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.test.BorderPatrolSuite
import com.twitter.finagle.http._
import com.twitter.finagle.memcached
import com.twitter.finagle.memcached.GetResult
import com.twitter.io.Buf
import com.twitter.util.{Await, Future, Time}


class BorderAuthSpec extends BorderPatrolSuite  {
  import com.lookout.borderpatrol.test.sessionx.helpers.{secretStore => store, _}

  // Method to decode SessionData from the sessionId in Response
  def sessionDataFromResponse(resp: Response): Future[Request] =
    (for {
      sessionId <- SignedId.fromResponse(resp).toFuture
      sessionMaybe <- sessionStore.get[Request](sessionId)
    } yield sessionMaybe.fold[Identity[Request]](EmptyIdentity)(s => Id(s.data))).map {
      case Id(req) => req
      case EmptyIdentity => null
    }

  //  Test Services
  val serviceFilterTestService = mkTestService[ServiceRequest, Response] { req => Future.value(Response(Status.Ok)) }
  val sessionIdFilterTestService = mkTestService[SessionIdRequest, Response] { req => Future.value(Response(Status.Ok)) }
  val identityFilterTestService = mkTestService[AccessIdRequest[Request], Response] { req => Future.value(Response(Status.Ok)) }
  val workingService = mkTestService[SessionIdRequest, Response] { req => Response(Status.Ok).toFuture }
  val workingMap = Map("keymaster" -> workingService)

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
    val output = (ServiceFilter(serviceMatcher) andThen serviceFilterTestService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "return NotFound Status if Request is destined to an unknown Service" in {
    // Execute
    val output = (ServiceFilter(serviceMatcher) andThen serviceFilterTestService)(req("foo", "/bar"))

    // Validate
    Await.result(output).status should be (Status.NotFound)
  }

  behavior of "SessionIdFilter"

  it should "succeed and return output of upstream Service if ServiceRequest contains SignedId" in {
    val testService = mkTestService[SessionIdRequest, Response] {
      req => {
        assert(req.req.path == "/ent")
        assert(req.serviceId == one)
        Future.value(Response(Status.Ok))
      }
    }

    //  Allocate and Session
    val sessionId = sessionid.untagged
    val cooki = sessionId.asCookie()

    //  Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)

    //  Execute
    val output = (SessionIdFilter(sessionStore) andThen testService)(ServiceRequest(request, cust1, one))

    //  Verify
    Await.result(output).status should be (Status.Ok)
  }

  it should "return redirect to login URI, if no SignedId present in the SessionIdRequest" in {

    // Create request
    val request = req("enterprise", "/ent")

    // Execute
    val output = (SessionIdFilter(sessionStore) andThen sessionIdFilterTestService)(ServiceRequest(request, cust1, one))

    // Validate
    Await.result(output).status should be (Status.Found)
    Await.result(output).location should be equals(
      cust1.loginManager.protoManager.redirectLocation(None))
    val sessionData = sessionDataFromResponse(Await.result(output))
    Await.result(sessionData).path should be equals(request.path)
  }

  it should "return redirect to login URI, if no SignedId present in the SessionIdRequest for OAuth2Code" in {

    // Create request
    val request = req("sky", "/umb")

    // Execute
    val output = (SessionIdFilter(sessionStore) andThen sessionIdFilterTestService)(ServiceRequest(request, cust2, two))

    // Validate
    Await.result(output).status should be (Status.Found)
    Await.result(output).location should be equals(
      cust2.loginManager.protoManager.redirectLocation(request.host))
    val sessionData = sessionDataFromResponse(Await.result(output))
    Await.result(sessionData).path should be equals(request.path)
  }

  it should "throw an exception if SignedId and Host are not present in the HTTP Request for OAuth2Code" in {

    // Create request
    val request = Request("/umb")

    // Execute
    val output = (SessionIdFilter(sessionStore) andThen sessionIdFilterTestService)(ServiceRequest(request, cust2, two))

    // Validate
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("Host not found in HTTP Request")
  }

  it should "propagate the error Status code returned by the upstream Service" in {
    val testService = mkTestService[SessionIdRequest, Response] { request => Future.value(Response(Status.NotFound))}

    // Allocate and Session
    val sessionId = sessionid.untagged
    val cooki = sessionId.asCookie()

    // Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)

    // Execute
    val output = (SessionIdFilter(sessionStore) andThen testService)(ServiceRequest(request, cust1, one))

    // Verify
    Await.result(output).status should be (Status.NotFound)
  }

  it should "propagate the Exception thrown by the upstream Service" in {
    val testService = mkTestService[SessionIdRequest, Response] {
      request => Future.exception(new Exception("SessionIdFilter test failure"))
    }

    // Allocate and Session
    val sessionId = sessionid.untagged
    val cooki = sessionId.asCookie()

    // Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)

    // Execute
    val output = (SessionIdFilter(sessionStore) andThen testService)(ServiceRequest(request, cust1, one))

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
    val output = (SessionIdFilter(mockSessionStore) andThen sessionIdFilterTestService)(ServiceRequest(request, cust1, one))

    // Verify
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("oopsie")
  }

  behavior of "IdentityFilter"

  it should "succeed and return output of upstream Service, if Session is found for SignedId" in {
    val testService = mkTestService[AccessIdRequest[Int], Response] {
      request => {
        assert(request.id == Identity(999))
        Future.value(Response(Status.Ok)) }
    }

    //  Allocate and Session
    val sessionId = sessionid.untagged
    val cooki = sessionId.asCookie()

    //  Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)
    val sessionData = 999
    sessionStore.update[Int](Session(sessionId, sessionData))

    //  Execute
    val output = (IdentityFilter[Int](sessionStore) andThen testService)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

    //  Verify
    Await.result(output).status should be (Status.Ok)
  }

  it should "return a redirect to login UTI, if it fails Session lookup using SignedId" in {

    // Allocate and Session
    val sessionId = sessionid.untagged
    val cooki = sessionId.asCookie()

    // Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)

    // Execute
    val output = (IdentityFilter[Request](sessionStore) andThen identityFilterTestService)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

    // Verify
    Await.result(output).status should be (Status.Found)
    Await.result(output).location should be equals(
      cust1.loginManager.protoManager.redirectLocation(None))
    val returnedSessionId = SignedId.fromResponse(Await.result(output)).toFuture
    Await.result(returnedSessionId) should not equals(sessionId)
    val sessionData = sessionDataFromResponse(Await.result(output))
    Await.result(sessionData).path should be equals(request.path)
  }

  it should "propagate the exception thrown by SessionStore.get operation" in {
    // Allocate and Session
    val sessionId = sessionid.untagged
    val cooki = sessionId.asCookie()

    // Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)

    // Mock sessionStore
    val mockSessionStore = MemcachedStore(FailingMockClient)

    // Execute
    val output = (IdentityFilter[Request](mockSessionStore) andThen identityFilterTestService)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

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
    val sessionId = sessionid.untagged
    val cooki = sessionId.asCookie()

    // Mock sessionStore
    val mockSessionStore = MemcachedStore(FailingUpdateMockClient)

    // Create request
    val request = req("enterprise", "/ent")
    request.addCookie(cooki)

    // Execute
    val output = (IdentityFilter[Request](mockSessionStore) andThen identityFilterTestService)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

    // Verify
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("whoopsie")
  }

  behavior of "ExceptionFilter"

  it should "succeed and act as a passthru for the valid Response returned by Service" in {
    val testService = mkTestService[Request, Response] { req => Future.value(Response(Status.Ok)) }

    // Execute
    val output = (ExceptionFilter() andThen testService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "succeed and convert the AccessDenied exception into error Response" in {
    val testService = mkTestService[Request, Response] { req =>
      Future.exception(AccessDenied(Status.NotAcceptable, "No access allowed to service"))
    }

    // Execute
    val output = (ExceptionFilter() andThen testService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.NotAcceptable)
    Await.result(output).contentString should be ("AccessDenied: No access allowed to service")
  }

  it should "succeed and convert the SessionStoreError exception into error Response" in {
    val testService = mkTestService[Request, Response] { req =>
      Future.exception(new SessionStoreError("update failed"))
    }

    // Execute
    val output = (ExceptionFilter() andThen testService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.InternalServerError)
    Await.result(output).contentString should be ("An error occurred interacting with the session store: update failed")
  }

  it should "succeed and convert the AccessIssuerError exception into error Response" in {
    val testService = mkTestService[Request, Response] { req =>
      Future.exception(AccessIssuerError(Status.NotAcceptable, "Some access issuer error"))
    }

    // Execute
    val output = (ExceptionFilter() andThen testService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.NotAcceptable)
    Await.result(output).contentString should be ("Some access issuer error")
  }

  it should "succeed and convert the IdentityProviderError exception into error Response" in {
    val testService = mkTestService[Request, Response] { req =>
      Future.exception(IdentityProviderError(Status.NotAcceptable, "Some identity provider error"))
    }

    // Execute
    val output = (ExceptionFilter() andThen testService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.NotAcceptable)
    Await.result(output).contentString should be ("Some identity provider error")
  }

  it should "succeed and convert the Runtime exception into error Response" in {
    val testService = mkTestService[Request, Response] { req =>
      Future.exception(new RuntimeException("some weird exception"))
    }

    // Execute
    val output = (ExceptionFilter() andThen testService)(req("enterprise", "/ent"))

    // Validate
    Await.result(output).status should be (Status.InternalServerError)
    Await.result(output).contentString should be ("some weird exception")
  }

  behavior of "BorderService"

  it should "successfully reach the upstream service path via access service chain, if authenticated" in {
    val identityService = mkTestService[SessionIdRequest, Response] { _ => fail("Must not invoke identity service") }
    val identityProviderMap = Map("keymaster" -> identityService)

    // Allocate and Session
    val sessionId = sessionid.authenticated

    // Login POST request
    val request = req("enterprise", "/ent")

    // Original request
    val output = BorderService(identityProviderMap, workingMap)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

    //  Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "successfully reach the loginManager path via identity service chain, if unauthenticated " in {
    val accessService = mkTestService[SessionIdRequest, Response] { _ => fail("Must not invoke identity service") }
    val accessServiceMap = Map("keymaster" -> accessService)

    // Allocate and Session
    val sessionId = sessionid.untagged

    // Login POST request
    val request = req("enterprise", "/check")

    // Original request
    val output = BorderService(workingMap, accessServiceMap)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

    //  Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "successfully reach the loginManager login post path via identity service chain, if unauthenticated" in {
    val accessService = mkTestService[SessionIdRequest, Response] { _ => fail("Must not invoke identity service") }
    val accessServiceMap = Map("keymaster" -> accessService)

    // Allocate and Session
    val sessionId = sessionid.untagged

    // Login POST request
    val request = req("enterprise", "/loginConfirm")

    // Original request
    val output = BorderService(workingMap, accessServiceMap)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

    //  Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "send a redirect if session is unauthenticated and trying to reach upstream service" in {
    val testService = mkTestService[SessionIdRequest, Response] { _ => fail("should not get here") }

    //  Allocate and Session
    val sessionId = sessionid.untagged

    //  Create request
    val request = req("enterprise", "/ent/dothis")

    //  Execute
    val output = BorderService(workingMap, workingMap)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

    //  Verify
    Await.result(output).status should be (Status.Found)
    Await.result(output).location.value should be ("/check")
  }

  it should "send a redirect if session is authenticated and trying to reach LoginManager login post Path" in {

    //  Allocate and Session
    val sessionId = sessionid.authenticated

    //  Create request
    val request = req("enterprise", "/loginConfirm")

    //  Execute
    val output = BorderService(workingMap, workingMap)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

    //  Verify
    Await.result(output).status should be (Status.Found)
    Await.result(output).location.value should be ("/ent")
  }

  it should "send a redirect if session is authenticated and trying to reach LoginManager path" in {
    val testService = mkTestService[SessionIdRequest, Response] { _ => fail("should not get here") }

    //  Allocate and Session
    val sessionId = sessionid.authenticated

    //  Create request
    val request = req("enterprise", "/check/something")

    //  Execute
    val output = BorderService(workingMap, workingMap)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

    //  Verify
    Await.result(output).status should be (Status.Found)
    Await.result(output).location.value should be ("/ent")
  }

  it should "throw an AccessIssuerError if it fails to find AccessIssuer service chain" in {
    val accessIssuerMap = Map("foo" -> workingService)

    // Allocate and Session
    val sessionId = sessionid.authenticated

    // Login POST request
    val request = req("enterprise", "/ent")

    // Validate
    val caught = the [AccessIssuerError] thrownBy {
      // Execute
      val output = BorderService(workingMap, accessIssuerMap)(
        SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))
    }
    caught.getMessage should equal ("Failed to find AccessIssuer Service Chain for keymaster")
  }

  it should "throw an IdentityProviderError if it fails to find IdentityProvider service chain" in {
    val identityProviderMap = Map("foo" -> workingService)

    // Allocate and Session
    val sessionId = sessionid.untagged

    // Login POST request
    val request = req("enterprise", "/check")

    // Validate
    val caught = the [IdentityProviderError] thrownBy {
      // Execute
      val output = BorderService(identityProviderMap, workingMap)(
        SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))
    }
    caught.getMessage should equal ("Failed to find IdentityProvider Service Chain for keymaster")
  }

  behavior of "LoginManagerFilter"

  it should "succeed and invoke the method on loginManager" in {
    val testService = mkTestService[SessionIdRequest, Response] { _ => fail("Should not get here") }
    val testLoginManagerBinder = mkTestLoginManagerBinder { _ => Response(Status.Ok).toFuture }

    // Allocate and Session
    val sessionId = sessionid.untagged

    // Create request
    val request = req("enterprise", "/check")

    // Execute
    val output = (LoginManagerFilter(testLoginManagerBinder) andThen testService)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "succeed and invoke the method on keymaster service" in {
    val testService = mkTestService[SessionIdRequest, Response] { _ => Response(Status.Ok).toFuture }
    val testLoginManagerBinder = mkTestLoginManagerBinder { _ => fail("Should not get here") }

    // Allocate and Session
    val sessionId = sessionid.untagged

    // Create request
    val request = Request(Method.Post, "/loginConfirm")

    // Execute
    val output = (LoginManagerFilter(testLoginManagerBinder) andThen testService)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "succeed and invoke the non GET or POST method on keymaster service" in {
    val testService = mkTestService[SessionIdRequest, Response] { _ => fail("Should not get here") }
    val testLoginManagerBinder = mkTestLoginManagerBinder { _ => Response(Status.Ok).toFuture }

    // Allocate and Session
    val sessionId = sessionid.untagged

    // Create request
    val request = Request(Method.Head, "/ent")

    // Execute
    val output = (LoginManagerFilter(testLoginManagerBinder) andThen testService)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  behavior of "AccessFilter"

  case class TestAccessResponse(access: Access[String]) extends AccessResponse[String]

  it should "succeed and include service token in the request and invoke the REST API of upstream service" in {
    val accessService = mkTestService[AccessRequest[Int], AccessResponse[String]] {
      request => TestAccessResponse (Access("blah")).toFuture
    }
    val testSidBinder = mkTestSidBinder {
      request => {
        // Verify service token in the request
        assert(request.req.uri == one.path.toString)
        assert(request.req.headerMap.get("Auth-Token") == Some("blah"))
        Response(Status.Ok).toFuture
      }
    }

    // Allocate and Session
    val sessionId = sessionid.authenticated

    // Create request
    val request = req("enterprise", "/ent")

    // Execute
    val output = (AccessFilter[Int, String](testSidBinder) andThen accessService)(
      AccessIdRequest(SessionIdRequest(ServiceRequest(request, cust1, one), sessionId), Id(10)))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  behavior of "RewriteFilter"

  it should "succeed and include service token in the request and invoke the REST API of upstream service" in {
    val testService = mkTestService[SessionIdRequest, Response] {
      req => {
        // Verify path is unchanged in the request
        assert(req.req.uri.startsWith(one.path.toString))
        Response(Status.Ok).toFuture
      }
    }

    // Allocate and Session
    val sessionId = sessionid.authenticated

    // Create request
    val request = req("enterprise", "/ent/whatever")

    // Execute
    val output = (RewriteFilter() andThen testService)(
      SessionIdRequest(ServiceRequest(request, cust1, one), sessionId))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "succeed and include service token in the request and invoke rewritten URL on upstream service" in {
    val testService = mkTestService[SessionIdRequest, Response] {
      req => {
        // Verify path is rewritten in the request
        assert(req.req.uri.startsWith(two.rewritePath.get.toString))
        Response(Status.Ok).toFuture
      }
    }

    // Allocate and Session
    val sessionId = sessionid.authenticated

    // Create request
    val request = req("umbrella", "/umb/some/weird/path")

    // Execute
    val output = (RewriteFilter() andThen testService)(
      SessionIdRequest(ServiceRequest(request, cust2, two), sessionId))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  behavior of "LogoutService"

  it should "succeed to logout by deleting session from store and redirecting to default service" in {
    // Allocate and Session
    val sessionId = sessionid.authenticated
    val cooki = sessionId.asCookie()

    // Session data
    val sessionData = 999
    sessionStore.update[Int](Session(sessionId, sessionData))

    // Create request
    val request = req("enterprise", "/logout")
    request.addCookie(cooki)

    // Execute
    val output = LogoutService(sessionStore).apply(ServiceRequest(request, cust1, one))

    // Validate
    Await.result(output).status should be (Status.Found)
    Await.result(output).location.get should be (cust1.defaultServiceId.path.toString)
    Await.result(output).cookies.get(SignedId.sessionIdCookieName).get.value should be ("")
    Await.result(output).cookies.get(SignedId.sessionIdCookieName).get.isDiscard should be (true)
    Await.result(sessionStore.get[Int](sessionId)) should be (None)
  }

  it should "succeed to logout the requests w/o sessionId by simply redirecting to default service" in {
    // Create request
    val request = req("enterprise", "/logout")

    // Execute
    val output = LogoutService(sessionStore).apply(ServiceRequest(request, cust1, one))

    // Validate
    Await.result(output).status should be (Status.Found)
    Await.result(output).location.get should be (cust1.defaultServiceId.path.toString)
    Await.result(output).cookies.get(SignedId.sessionIdCookieName) should be (None)
  }
}
