package com.lookout.borderpatrol.auth.keymaster

import java.net.URL

import com.lookout.borderpatrol.Binder._
import com.lookout.borderpatrol.auth.keymaster.Keymaster._
import com.lookout.borderpatrol.auth._
import com.lookout.borderpatrol.sessionx.SessionStores.MemcachedStore
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.{LoginManager, Manager, ServiceMatcher, ServiceIdentifier}
import com.lookout.borderpatrol.test._
import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.finagle.memcached.GetResult
import com.twitter.finagle.{memcached, Service}
import com.twitter.finagle.httpx.path.Path
import com.twitter.finagle.httpx.{Method, Request, Status, Response}
import com.twitter.util.{Await, Future}


class KeymasterSpec extends BorderPatrolSuite  {
  import sessionx.helpers.{secretStore => store, _}
  import Tokens._

  val urls = Set(new URL("http://localhost:5678"))

  //  Managers
  val keymasterIdManager = Manager("keymaster", Path("/identityProvider"), urls)
  val keymasterAccessManager = Manager("keymaster", Path("/accessIssuer"), urls)
  val checkpointLoginManager = LoginManager("checkpoint", Path("/check"), urls, Path("/loginConfirm"),
    keymasterIdManager, keymasterAccessManager)

  // sids
  val one = ServiceIdentifier("one", urls, Path("/ent"), "enterprise", checkpointLoginManager)
  val serviceMatcher = ServiceMatcher(Set(one))
  val sessionStore = SessionStores.InMemoryStore

  // Request helper
  def req(subdomain: String, path: String, params: Tuple2[String, String]*): Request =
    Request(s"http://${subdomain + "."}example.com${path.toString}", params:_*)

  //  Tokens
  val serviceToken2 = ServiceToken("SomeServiceTokenData2")
  val serviceTokens = ServiceTokens().add("service1", ServiceToken("SomeServiceTokenData1"))
  val tokens = Tokens(MasterToken("masterT"), serviceTokens)
  val tokens2 = tokens.add("one", serviceToken2)

  // Binders
  case class TestManagerBinder() extends MBinder[Manager]
  def mkTestManagerBinder(f: (BindRequest[Manager]) => Future[Response]): TestManagerBinder = new TestManagerBinder {
    override def apply(request: BindRequest[Manager]) = f(request)
  }
  case class TestLoginManagerBinder() extends MBinder[LoginManager]
  def mkTestLoginManagerBinder(f: (BindRequest[LoginManager]) => Future[Response]): TestLoginManagerBinder =
    new TestLoginManagerBinder {
      override def apply(request: BindRequest[LoginManager]) = f(request)
    }
  case class TestServiceIdentifierBinder() extends MBinder[ServiceIdentifier]
  def mkTestSidBinder(f: (BindRequest[ServiceIdentifier]) => Future[Response]): TestServiceIdentifierBinder =
    new TestServiceIdentifierBinder {
      override def apply(request: BindRequest[ServiceIdentifier]) = f(request)
  }

  // Method to decode SessionData from the sessionId
  def getTokensFromSessionId(sid: SessionId): Future[Tokens] =
    (for {
      sessionMaybe <- sessionStore.get[Tokens](sid)
    } yield sessionMaybe.fold[Identity[Tokens]](EmptyIdentity)(s => Id(s.data))).map(i => i match {
      case Id(tokens) => tokens
      case EmptyIdentity => null
    })

  // Method to decode SessionData from the sessionId in Response
  def sessionDataFromResponse(resp: Response): Future[Tokens] =
    for {
      sessionId <- SessionId.fromResponse(resp).toFuture
      toks <- getTokensFromSessionId(sessionId)
    } yield toks

  // Test services
  def mkTestService[A, B](f: (A) => Future[B]) : Service[A, B] = new Service[A, B] {
    def apply(request: A) = f(request)
  }
  val keymasterLoginFilterTestService = mkTestService[IdentifyRequest[Credential], IdentifyResponse[Tokens]] {
    req => Future(KeymasterIdentifyRes(tokens)) }
  val keymasterTestService = mkTestService[Request, Response] { req => Response(Status.Ok).toFuture }

  behavior of "KeymasterIdentityProvider"

  it should "succeed and return IdentityResponse with tokens received from upstream Keymaster Service" in {
    val testIdentityManagerBinder = mkTestManagerBinder { request => {
      assert(request.req.path == one.loginManager.identityManager.path.toString)
      tap(Response(Status.Ok))(res => {
        res.contentString = TokensEncoder(tokens).toString()
        res.contentType = "application/json"
      }).toFuture
    }}

    // Execute
    val output = KeymasterIdentityProvider(testIdentityManagerBinder)(KeymasterIdentifyReq(Credential("foo", "bar", one)))

    // Validate
    Await.result(output).identity should be (Id(tokens))
  }

  it should "propagate the error Status code from Keymaster service in the IdentityProviderError exception" in {
    val testIdentityManagerBinder = mkTestManagerBinder { request => Response(Status.NotFound).toFuture }

    // Execute
    val output = KeymasterIdentityProvider(testIdentityManagerBinder)(KeymasterIdentifyReq(Credential("foo", "bar", one)))

    // Validate
    val caught = the [IdentityProviderError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("Invalid credentials for user foo")
    caught.status should be (Status.NotFound)
  }

  it should "propagate the failure parsing the response from Keymaster service as an IdentityProviderError exception" in {
    val testIdentityManagerBinder = mkTestManagerBinder {
      request => tap(Response(Status.Ok))(res => {
        res.contentString = """{"key":"data"}"""
        res.contentType = "application/json"
      }).toFuture
    }

    // Execute
    val output = KeymasterIdentityProvider(testIdentityManagerBinder)(KeymasterIdentifyReq(Credential("foo", "bar", one)))

    // Validate
    val caught = the [IdentityProviderError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("Failed to parse the Keymaster Identity Response")
    caught.status should be (Status.InternalServerError)
  }

  behavior of "KeymasterPostLoginFilter"

  it should "succeed and saves tokens, sends redirect with tokens returned by IDP" in {
    val testService = mkTestService[IdentifyRequest[Credential], IdentifyResponse[Tokens]] {
      request => Future(KeymasterIdentifyRes(tokens))
    }

    // Allocate and Session
    val sessionId = sessionid.next

    // Login POST request
    val loginRequest = req("enterprise", "/login", ("username" -> "foo"), ("password" -> "bar"))

    // Original request
    val origReq = req("enterprise", "/dang", ("fake" -> "drake"))
    sessionStore.update[Request](Session(sessionId, origReq))

    // Execute
    val output = (KeymasterPostLoginFilter(sessionStore) andThen testService)(
      SessionIdRequest(ServiceRequest(loginRequest, one), sessionId))

    // Validate
    Await.result(output).status should be (Status.Found)
    Await.result(output).location should be equals ("/dang")
    val returnedSessionId = SessionId.fromResponse(Await.result(output)).toFuture
    returnedSessionId should not be sessionId
    val session_d = sessionStore.get[Tokens](Await.result(returnedSessionId))
    val tokensz = sessionDataFromResponse(Await.result(output))
    Await.result(tokensz) should be (tokens)
  }

  it should "return BadRequest Status if credentials are not present in the request" in {
    // Allocate and Session
    val sessionId = sessionid.next

    // Login POST request
    val loginRequest = req("enterprise", "/login", ("username" -> "foo"))

    // Execute
    val output = (KeymasterPostLoginFilter(sessionStore) andThen keymasterLoginFilterTestService)(
      SessionIdRequest(ServiceRequest(loginRequest, one), sessionId))

    // Validate
    Await.result(output).status should be (Status.BadRequest)
  }

  it should "return OriginalRequestNotFound if it fails find the original request from sessionStore" in {
    // Allocate and Session
    val sessionId = sessionid.next

    // Login POST request
    val loginRequest = req("enterprise", "/login", ("username" -> "foo"), ("password" -> "bar"))

    // Execute
    val output = (KeymasterPostLoginFilter(sessionStore) andThen keymasterLoginFilterTestService)(
      SessionIdRequest(ServiceRequest(loginRequest, one), sessionId))

    // Validate
    val caught = the [OriginalRequestNotFound] thrownBy {
      Await.result(output)
    }
  }

  it should "propagate the Exception thrown by Session lookup operation" in {
    //  Mock SessionStore client
    case object FailingMockClient extends memcached.MockClient {
      override def getResult(keys: Iterable[String]): Future[GetResult] = {
        Future.exception(new Exception("oopsie"))
      }
    }

    // Mock sessionStore
    val mockSessionStore = MemcachedStore(FailingMockClient)

    // Allocate and Session
    val sessionId = sessionid.next

    // Login POST request
    val loginRequest = req("enterprise", "/login", ("username" -> "foo"), ("password" -> "bar"))

    // Execute
    val output = (KeymasterPostLoginFilter(mockSessionStore) andThen keymasterLoginFilterTestService)(
      SessionIdRequest(ServiceRequest(loginRequest, one), sessionId))

    // Validate
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("oopsie")
  }

  behavior of "KeymasterAccessIssuer"

  it should "succeed, return service token found in the ServiceTokens cache" in {
    val testAccessManagerBinder = mkTestManagerBinder {
      request => { assert(false); Response(Status.Ok).toFuture }
    }
    val sessionId = sessionid.next

    // Execute
    val output = KeymasterAccessIssuer(testAccessManagerBinder, sessionStore)(KeymasterAccessReq(Id(tokens2), one, sessionId))

    // Validate
    Await.result(output).access.access should be (serviceToken2)
  }

  it should "succeed, save in SessionStore and return the ServiceToken received from the Keymaster Service" in {
    val testAccessManagerBinder = mkTestManagerBinder { request => {
      assert(request.req.path == one.loginManager.accessManager.path.toString)
      tap(Response(Status.Ok))(res => {
        res.contentString = TokensEncoder(tokens2).toString()
        res.contentType = "application/json"
      }).toFuture
    }}
    val sessionId = sessionid.next
    sessionStore.update[Tokens](Session(sessionId, tokens))

    // Execute
    val output = KeymasterAccessIssuer(testAccessManagerBinder, sessionStore)(KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    Await.result(output).access.access should be (serviceToken2)
    val tokIt = getTokensFromSessionId(sessionId)
    Await.result(tokIt) should be (tokens2)
  }

  it should "propagate the error Status code returned by the Keymaster service, as the AccessIssuerError exception" in {
    val testAccessManagerBinder = mkTestManagerBinder { request => Response(Status.NotFound).toFuture }
    val sessionId = sessionid.next

    // Execute
    val output = KeymasterAccessIssuer(testAccessManagerBinder, sessionStore)(KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    val caught = the [AccessIssuerError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("No access allowed to service one due to error: Status(404)")
    caught.status should be (Status.NotFound)
  }

  it should "propagate the failure to parse response content from Keymaster service, as AccessIssuerError exception" in {
    val testAccessManagerBinder = mkTestManagerBinder {
      request => tap(Response(Status.Ok))(res => {
        res.contentString = "invalid string"
        res.contentType = "application/json"
      }).toFuture
    }
    val sessionId = sessionid.next

    // Execute
    val output = KeymasterAccessIssuer(testAccessManagerBinder, sessionStore)(KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    val caught = the [AccessIssuerError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("Failed to parse the Keymaster Access Response")
  }

  it should "return an AccessDenied exception, if it fails to find the ServiceToken in the Keymaster response" in {
    val testAccessManagerBinder = mkTestManagerBinder {
      request => tap(Response(Status.Ok))(res => {
        res.contentString = TokensEncoder(tokens).toString()
        res.contentType = "application/json"
      }).toFuture
    }
    val sessionId = sessionid.next

    // Execute
    val output = KeymasterAccessIssuer(testAccessManagerBinder, sessionStore)(KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    val caught = the [AccessDenied] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("No access allowed to service one")
    caught.status should be (Status.NotAcceptable)
  }

  behavior of "KeymasterAccessFilter"

  it should "succeed and include service token in the request and invoke the REST API of upstream service" in {
    val accessService = mkTestService[AccessRequest[Tokens], AccessResponse[ServiceToken]] {
      request => KeymasterAccessRes(Access(serviceToken2)).toFuture
    }
    val testSidBinder = mkTestSidBinder {
      request => {
        // Verify service token in the request
        assert (request.req.headerMap.get("Auth-Token") == Some(serviceToken2.value))
        Response(Status.Ok).toFuture
      }
    }

    // Allocate and Session
    val sessionId = sessionid.next

    // Create request
    val request = req("enterprise", "/dang")

    // Execute
    val output = (KeymasterAccessFilter(testSidBinder) andThen accessService)(
      AccessIdRequest(SessionIdRequest(ServiceRequest(request, one), sessionId), Id(tokens)))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "propagate the failure status code returned by upstream service" in {
    val accessService = mkTestService[AccessRequest[Tokens], AccessResponse[ServiceToken]] {
      request => KeymasterAccessRes(Access(serviceToken2)).toFuture
    }
    val testSidBinder = mkTestSidBinder {
      request => {
        // Verify service token in the request
        assert (request.req.headerMap.get("Auth-Token") == Some(serviceToken2.value))
        Response(Status.NotFound).toFuture
      }
    }

    // Allocate and Session
    val sessionId = sessionid.next

    // Create request
    val request = req("enterprise", "/dang")

    // Execute
    val output = (KeymasterAccessFilter(testSidBinder) andThen accessService)(
      AccessIdRequest(SessionIdRequest(ServiceRequest(request, one), sessionId), Id(tokens)))

    // Validate
    Await.result(output).status should be (Status.NotFound)
  }

  it should "propagate the exception returned by Access Issuer Service" in {
    val accessService = mkTestService[AccessRequest[Tokens], AccessResponse[ServiceToken]] {
      request => Future.exception(new Exception("Oopsie"))
    }
    val testSidBinder = mkTestSidBinder {
      request => {
        // Verify service token in the request
        assert (request.req.headerMap.get("Auth-Token") == Some(serviceToken2.value))
        Response(Status.NotFound).toFuture
      }
    }

    // Allocate and Session
    val sessionId = sessionid.next

    // Create request
    val request = req("enterprise", "/dang")

    // Execute
    val output = (KeymasterAccessFilter(testSidBinder) andThen accessService)(
      AccessIdRequest(SessionIdRequest(ServiceRequest(request, one), sessionId), Id(tokens)))

    // Validate
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
  }

  behavior of "KeymasterMethodMuxLoginFilter"

  it should "succeed and invoke the GET on keymaster service" in {
    val testService = mkTestService[SessionIdRequest, Response] { _ => fail("Should not get here") }
    val testLoginManagerBinder = mkTestLoginManagerBinder { _ => Response(Status.Ok).toFuture }

    // Allocate and Session
    val sessionId = sessionid.next

    // Create request
    val request = Request(Method.Get, "/ent")

    // Execute
    val output = (KeymasterMethodMuxLoginFilter(testLoginManagerBinder) andThen testService)(
      SessionIdRequest(ServiceRequest(request, one), sessionId))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "succeed and invoke the POST on upstream service" in {
    val testService = mkTestService[SessionIdRequest, Response] { _ => Response(Status.Ok).toFuture }
    val testLoginManagerBinder = mkTestLoginManagerBinder { _ => fail("Should not get here") }

    // Allocate and Session
    val sessionId = sessionid.next

    // Create request
    val request = Request(Method.Post, "/ent")

    // Execute
    val output = (KeymasterMethodMuxLoginFilter(testLoginManagerBinder) andThen testService)(
      SessionIdRequest(ServiceRequest(request, one), sessionId))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "succeed and invoke the non GET or POST method on keymaster service" in {
    val testService = mkTestService[SessionIdRequest, Response] { _ => fail("Should not get here") }
    val testLoginManagerBinder = mkTestLoginManagerBinder { _ => Response(Status.Ok).toFuture }

    // Allocate and Session
    val sessionId = sessionid.next

    // Create request
    val request = Request(Method.Head, "/ent")

    // Execute
    val output = (KeymasterMethodMuxLoginFilter(testLoginManagerBinder) andThen testService)(
      SessionIdRequest(ServiceRequest(request, one), sessionId))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  behavior of "keymasterIdentityProviderChain"

  it should "succeed and invoke the GET on loginManager" in {
    val server = com.twitter.finagle.Httpx.serve(
      "localhost:5678", mkTestService[Request, Response]{request =>
        if (request.path.contains(checkpointLoginManager.path.toString)) Response(Status.Ok).toFuture
        else Response(Status.BadRequest).toFuture
      })

    try {
      // Allocate and Session
      val sessionId = sessionid.next

      // Login manager request
      val loginRequest = req("enterprise", checkpointLoginManager.path.toString,
        ("username" -> "foo"), ("password" -> "bar"))

      // Original request
      val origReq = req("enterprise", "/ent", ("fake" -> "drake"))
      sessionStore.update[Request](Session(sessionId, origReq))

      // Execute
      val output = (keymasterIdentityProviderChain(sessionStore)(store))(
        SessionIdRequest(ServiceRequest(loginRequest, one), sessionId))

      // Validate
      Await.result(output).status should be(Status.Ok)
    } finally {
      server.close()
    }
  }

  it should "succeed and invoke the GET on identityManager" in {
    val server = com.twitter.finagle.Httpx.serve(
      "localhost:5678", mkTestService[Request, Response]{request =>
        if (request.path.contains(keymasterIdManager.path.toString))
          tap(Response(Status.Ok))(res => {
            res.contentString = TokensEncoder(tokens).toString()
            res.contentType = "application/json"
          }).toFuture
        else Response(Status.BadRequest).toFuture
      })

    try {
      // Allocate and Session
      val sessionId = sessionid.next

      // Login manager request
      val loginRequest = Request(Method.Post, Request.queryString(checkpointLoginManager.loginPath.toString,
        ("username" -> "foo"), ("password" -> "bar")))

      // Original request
      val origReq = req("enterprise", "/ent", ("fake" -> "drake"))
      sessionStore.update[Request](Session(sessionId, origReq))

      // Execute
      val output = (keymasterIdentityProviderChain(sessionStore)(store))(
        SessionIdRequest(ServiceRequest(loginRequest, one), sessionId))

      // Validate
      Await.result(output).status should be(Status.Found)
    } finally {
      server.close()
    }
  }

  behavior of "keymasterAccessIssuerChain"

  it should "succeed and invoke the GET on accessManager" in {
    val server = com.twitter.finagle.Httpx.serve(
      "localhost:5678", mkTestService[Request, Response]{request =>
        if (request.path.contains(keymasterAccessManager.path.toString))
          tap(Response(Status.Ok))(res => {
            res.contentString = TokensEncoder(tokens2).toString()
            res.contentType = "application/json"
          }).toFuture
        else if (request.path.contains(one.path.toString)) Response(Status.Ok).toFuture
        else Response(Status.BadRequest).toFuture
      })

    try {
      // Allocate and Session
      val sessionId = sessionid.next

      // Original request
      val origReq = req("enterprise", "/ent")
      sessionStore.update[Tokens](Session(sessionId, tokens))

      // Execute
      val output = (keymasterAccessIssuerChain(sessionStore)(store))(
        SessionIdRequest(ServiceRequest(origReq, one), sessionId))

      // Validate
      Await.result(output).status should be(Status.Ok)
    } finally {
      server.close()
    }
  }
}
