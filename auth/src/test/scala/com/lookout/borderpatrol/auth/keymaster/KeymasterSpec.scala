package com.lookout.borderpatrol.auth.keymaster

import java.net.URL

import com.lookout.borderpatrol.Binder._
import com.lookout.borderpatrol.auth.keymaster.Keymaster._
import com.lookout.borderpatrol.auth._
import com.lookout.borderpatrol.sessionx.SessionStores.MemcachedStore
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.{ServiceIdentifier, ServiceMatcher, CommunicationError}
import com.lookout.borderpatrol.{LoginManager, InternalAuthProtoManager, Manager, OAuth2CodeProtoManager}
import com.lookout.borderpatrol.test._
import com.lookout.borderpatrol.util.Combinators.tap
import com.nimbusds.jwt.{PlainJWT, JWTClaimsSet}
import com.twitter.finagle.memcached.GetResult
import com.twitter.finagle.{memcached, Service}
import com.twitter.finagle.httpx.path.Path
import com.twitter.finagle.httpx._
import com.twitter.util.{Await, Future}


class KeymasterSpec extends BorderPatrolSuite  {
  import sessionx.helpers.{secretStore => store, _}
  import Tokens._
  import KeymasterTransform._

  val urls = Set(new URL("http://localhost:5678"))

  //  Managers
  val keymasterIdManager = Manager("keymaster", Path("/identityProvider"), urls)
  val keymasterAccessManager = Manager("keymaster", Path("/accessIssuer"), urls)
  val internalProtoManager = InternalAuthProtoManager(Path("/loginConfirm"), Path("/check"), urls)
  val checkpointLoginManager = LoginManager("checkpoint", keymasterIdManager, keymasterAccessManager,
    internalProtoManager)
  val oauth2CodeProtoManager = OAuth2CodeProtoManager(Path("/signin"),
    new URL("http://example.com/authorizeUrl"),
    new URL("http://localhost:4567/tokenUrl"), "clientId", "clientSecret")
  val umbrellaLoginManager = LoginManager("ulm", keymasterIdManager, keymasterAccessManager,
    oauth2CodeProtoManager)
  val oauth2CodeBadProtoManager = OAuth2CodeProtoManager(Path("/signblew"),
    new URL("http://localhost:9999/authorizeUrl"),
    new URL("http://localhost:9999/tokenUrl"), "clientId", "clientSecret")
  val rainyLoginManager = LoginManager("rlm", keymasterIdManager, keymasterAccessManager,
    oauth2CodeBadProtoManager)

  // sids
  val one = ServiceIdentifier("one", urls, Path("/ent"), None, "enterprise", checkpointLoginManager)
  val two = ServiceIdentifier("two", urls, Path("/umb"), Some(Path("/broken/umb")), "umbrella", umbrellaLoginManager)
  val three = ServiceIdentifier("three", urls, Path("/rain"), None, "rainy", rainyLoginManager)
  val serviceMatcher = ServiceMatcher(Set(one, two, three))
  val sessionStore = SessionStores.InMemoryStore

  // Request helper
  def req(subdomain: String, path: String, params: Tuple2[String, String]*): Request =
    RequestBuilder().url(s"http://${subdomain + "."}example.com${Request.queryString(path, params:_*)}").buildGet()

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
    val output = KeymasterIdentityProvider(testIdentityManagerBinder).apply(
      KeymasterIdentifyReq(InternalAuthCredential("foo", "bar", one)))

    // Validate
    Await.result(output).identity should be (Id(tokens))
  }

  it should "propagate the error Status code from Keymaster service in the IdentityProviderError exception" in {
    val testIdentityManagerBinder = mkTestManagerBinder { request => Response(Status.NotFound).toFuture }

    // Execute
    val output = KeymasterIdentityProvider(testIdentityManagerBinder).apply(
      KeymasterIdentifyReq(OAuth2CodeCredential("foo", "bar", two)))

    // Validate
    val caught = the [IdentityProviderError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("Invalid credentials for user foo")
    caught.status should be (Status.NotFound)
  }

  it should "propagate the failure parsing the resp from Keymaster service as an IdentityProviderError exception" in {
    val testIdentityManagerBinder = mkTestManagerBinder {
      request => tap(Response(Status.Ok))(res => {
        res.contentString = """{"key":"data"}"""
        res.contentType = "application/json"
      }).toFuture
    }

    // Execute
    val output = KeymasterIdentityProvider(testIdentityManagerBinder).apply(
      KeymasterIdentifyReq(InternalAuthCredential("foo", "bar", one)))

    // Validate
    val caught = the [IdentityProviderError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("Failed to parse the Keymaster Identity Response")
    caught.status should be (Status.InternalServerError)
  }

  behavior of "transformOAuth2"

  it should "succeed and exchange code for token with OAuth2 IDP" in {
    val accessToken = new PlainJWT(new JWTClaimsSet.Builder().subject("SomethingAccess").build)
    val idToken = new PlainJWT(new JWTClaimsSet.Builder().subject("abc123").claim("upn", "test@example.com").build)
    val aadToken = AadToken(accessToken.serialize, idToken.serialize())
    val server = com.twitter.finagle.Httpx.serve(
      "localhost:4567", mkTestService[Request, Response] { req =>
        assert(req.getParam("code") == "XYZ123")
        tap(Response(Status.Ok))(res => {
          res.contentString = AadTokenEncoder(aadToken).toString()
          res.contentType = "application/json"
        }).toFuture
      })
    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // Execute
      val output = two.loginManager.protoManager.transform[SessionIdRequest, OAuth2CodeCredential](
        SessionIdRequest(ServiceRequest(loginRequest, two), sessionId))

      // Validate
      Await.result(output).uniqueId should be("test@example.com")
      Await.result(output).subject should be("abc123")
    } finally {
      server.close()
    }
  }

  it should "throw a CommunicationError on failure to talk with OAuth2 IDP" in {

    // Allocate and Session
    val sessionId = sessionid.untagged

    // Login POST request
    val loginRequest = req("rainy", "/signblew", ("code" -> "XYZ123"))

    // Execute
    val output = three.loginManager.protoManager.transform[SessionIdRequest, OAuth2CodeCredential](
      SessionIdRequest(ServiceRequest(loginRequest, three), sessionId))

    // Validate
    val caught = the [CommunicationError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should startWith ("An error occurred while talking to: " +
      "http://localhost:9999/tokenUrl with java.net.ConnectException: Connection refused:")
  }

  it should "throw an Exception on receiving HTTP request with no host in it" in {

    // Allocate and Session
    val sessionId = sessionid.untagged

    // Login POST request
    val loginRequest = Request("/signin", ("code" -> "XYZ123"))

    // Execute
    // Validate
    val caught = the [Exception] thrownBy {
      val output = two.loginManager.protoManager.transform[SessionIdRequest, OAuth2CodeCredential](
        SessionIdRequest(ServiceRequest(loginRequest, two), sessionId))
    }
    caught.getMessage should be ("Host not found in HTTP Request")
  }

  it should "throw an exception if fails to parse OAuth Server response" in {
    val server = com.twitter.finagle.Httpx.serve(
      "localhost:4567", mkTestService[Request, Response] { req =>
        assert(req.getParam("code") == "XYZ123")
        tap(Response(Status.Ok))(res => {
          res.contentString = """{"key":"value"}"""
          res.contentType = "application/json"
        }).toFuture
      })
    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // Execute
      val output = two.loginManager.protoManager.transform[SessionIdRequest, OAuth2CodeCredential](
        SessionIdRequest(ServiceRequest(loginRequest, two), sessionId))

      // Validate
      val caught = the [IdentityProviderError] thrownBy {
        Await.result(output)
      }
      caught.getMessage should be ("Failed to parse the AadToken received from OAuth2 Server: ulm")
      caught.status should be (Status.InternalServerError)
    } finally {
      server.close()
    }
  }

  it should "throw an exception if OAuth Server returns an failure status" in {
    val server = com.twitter.finagle.Httpx.serve(
      "localhost:4567", mkTestService[Request, Response] { req =>
        assert(req.getParam("code") == "XYZ123")
        Response(Status.NotAcceptable).toFuture
      })
    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // Execute
      val output = two.loginManager.protoManager.transform[SessionIdRequest, OAuth2CodeCredential](
        SessionIdRequest(ServiceRequest(loginRequest, two), sessionId))

      // Validate
      val caught = the [IdentityProviderError] thrownBy {
        Await.result(output)
      }
      caught.getMessage should be ("Failed to receive the AadToken from OAuth2 Server: ulm")
      caught.status should be (Status.NotAcceptable)
    } finally {
      server.close()
    }
  }

  it should "throw an IdentityProviderError if it fails to parse ID token" in {
    val accessToken = new PlainJWT(new JWTClaimsSet.Builder().subject("SomethingAccess").build)
    val idToken = "stuff" //"""{"key":"value"}"""
    val aadToken = AadToken(accessToken.serialize(), idToken)
    val server = com.twitter.finagle.Httpx.serve(
      "localhost:4567", mkTestService[Request, Response] { req =>
        assert(req.getParam("code") == "XYZ123")
        tap(Response(Status.Ok))(res => {
          res.contentString = AadTokenEncoder(aadToken).toString()
          res.contentType = "application/json"
        }).toFuture
      })
    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // Execute
      val output = two.loginManager.protoManager.transform[SessionIdRequest, OAuth2CodeCredential](
        SessionIdRequest(ServiceRequest(loginRequest, two), sessionId))

      // Validate
      val caught = the [TokenParsingError] thrownBy {
        Await.result(output)
      }
      caught.getMessage should startWith ("Failed to parse token with: Invalid serialized")
    } finally {
      server.close()
    }
  }

  behavior of "KeymasterPostLoginFilter"

  it should "succeed and saves tokens for internal auth, sends redirect with tokens returned by keymaster IDP" in {
    val testService = mkTestService[IdentifyRequest[Credential], IdentifyResponse[Tokens]] {
      request => Future(KeymasterIdentifyRes(tokens))
    }

    // Allocate and Session
    val sessionId = sessionid.untagged

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

  it should "succeed and saves tokens for AAD auth, sends redirect with tokens returned by keymaster IDP" in {
    val accessToken = new PlainJWT(new JWTClaimsSet.Builder().subject("SomethingAccess").build)
    val idToken = new PlainJWT(new JWTClaimsSet.Builder().subject("abc123").claim("upn", "test@example.com").build)
    val aadToken = AadToken(accessToken.serialize, idToken.serialize())
    val server = com.twitter.finagle.Httpx.serve(
      "localhost:4567", mkTestService[Request, Response] { req =>
        assert(req.getParam("code") == "XYZ123")
        tap(Response(Status.Ok))(res => {
          res.contentString = AadTokenEncoder(aadToken).toString()
          res.contentType = "application/json"
        }).toFuture
      })
    try {
      val testService = mkTestService[IdentifyRequest[Credential], IdentifyResponse[Tokens]] {
        request =>
          assert(request.credential.uniqueId == "test@example.com")
          Future.value(KeymasterIdentifyRes(tokens))
      }

      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login POST request
      val loginRequest = req("umbrella", "/signin", ("code" -> "XYZ123"))

      // Original request
      val origReq = req("umbrella", "/umb", ("fake" -> "drake"))
      sessionStore.update[Request](Session(sessionId, origReq))

      // Execute
      val output = (KeymasterPostLoginFilter(sessionStore) andThen testService)(
        SessionIdRequest(ServiceRequest(loginRequest, two), sessionId))

      // Validate
      Await.result(output).status should be(Status.Found)
      Await.result(output).location should be equals ("/umb")
      val returnedSessionId = SessionId.fromResponse(Await.result(output)).toFuture
      returnedSessionId should not be sessionId
      val session_d = sessionStore.get[Tokens](Await.result(returnedSessionId))
      val tokensz = sessionDataFromResponse(Await.result(output))
      Await.result(tokensz) should be(tokens)
    } finally {
      server.close()
    }
  }

  it should "return BadRequest Status if credentials are not present in the request" in {
    // Allocate and Session
    val sessionId = sessionid.untagged

    // Login POST request
    val loginRequest = req("enterprise", "/login", ("username" -> "foo"))

    // Execute
    val output = (KeymasterPostLoginFilter(sessionStore) andThen keymasterLoginFilterTestService)(
      SessionIdRequest(ServiceRequest(loginRequest, one), sessionId))

    // Validate
    val caught = the [IdentityProviderError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("transformBasic: Failed to parse the Request")
  }

  it should "return OriginalRequestNotFound if it fails find the original request from sessionStore" in {
    // Allocate and Session
    val sessionId = sessionid.untagged

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
    val sessionId = sessionid.untagged

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
    val sessionId = sessionid.untagged

    // Execute
    val output = KeymasterAccessIssuer(testAccessManagerBinder, sessionStore).apply(
      KeymasterAccessReq(Id(tokens2), one, sessionId))

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
    val sessionId = sessionid.untagged
    sessionStore.update[Tokens](Session(sessionId, tokens))

    // Execute
    val output = KeymasterAccessIssuer(testAccessManagerBinder, sessionStore).apply(
      KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    Await.result(output).access.access should be (serviceToken2)
    val tokIt = getTokensFromSessionId(sessionId)
    Await.result(tokIt) should be (tokens2)
  }

  it should "propagate the error Status code returned by the Keymaster service, as the AccessIssuerError exception" in {
    val testAccessManagerBinder = mkTestManagerBinder { request => Response(Status.NotFound).toFuture }
    val sessionId = sessionid.untagged

    // Execute
    val output = KeymasterAccessIssuer(testAccessManagerBinder, sessionStore).apply(
      KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    val caught = the [AccessIssuerError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("No access allowed to service one due to error: Status(404)")
    caught.status should be (Status.NotFound)
  }

  it should "propagate the failure to parse resp content from Keymaster service, as AccessIssuerError exception" in {
    val testAccessManagerBinder = mkTestManagerBinder {
      request => tap(Response(Status.Ok))(res => {
        res.contentString = "invalid string"
        res.contentType = "application/json"
      }).toFuture
    }
    val sessionId = sessionid.untagged

    // Execute
    val output = KeymasterAccessIssuer(testAccessManagerBinder, sessionStore).apply(
      KeymasterAccessReq(Id(tokens), one, sessionId))

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
    val sessionId = sessionid.untagged

    // Execute
    val output = KeymasterAccessIssuer(testAccessManagerBinder, sessionStore).apply(
      KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    val caught = the [AccessDenied] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("No access allowed to service one")
    caught.status should be (Status.NotAcceptable)
  }

  behavior of "AccessFilter"

  it should "succeed and include service token in the request and invoke the REST API of upstream service" in {
    val accessService = mkTestService[AccessRequest[Tokens], AccessResponse[ServiceToken]] {
      request => KeymasterAccessRes(Access(serviceToken2)).toFuture
    }
    val testSidBinder = mkTestSidBinder {
      request => {
        // Verify service token in the request
        assert(request.req.headerMap.get("Auth-Token") == Some(serviceToken2.value))
        Response(Status.Ok).toFuture
      }
    }

    // Allocate and Session
    val sessionId = sessionid.authenticated

    // Create request
    val request = req("enterprise", "/ent")

    // Execute
    val output = (AccessFilter[Tokens, ServiceToken](testSidBinder) andThen accessService)(
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
    val sessionId = sessionid.authenticated

    // Create request
    val request = req("enterprise", "/ent/whatever")

    // Execute
    val output = (AccessFilter[Tokens, ServiceToken](testSidBinder) andThen accessService)(
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
    val sessionId = sessionid.authenticated

    // Create request
    val request = req("enterprise", "/ent/something")

    // Execute
    val output = (AccessFilter[Tokens, ServiceToken](testSidBinder) andThen accessService)(
      AccessIdRequest(SessionIdRequest(ServiceRequest(request, one), sessionId), Id(tokens)))

    // Validate
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
  }

  behavior of "keymasterIdentityProviderChain"

  it should "succeed and invoke the GET on loginManager" in {
    val server = com.twitter.finagle.Httpx.serve(
      "localhost:5678", mkTestService[Request, Response]{request =>
        if (request.path.contains("check")) Response(Status.Ok).toFuture
        else Response(Status.BadRequest).toFuture
      })

    try {
      // Allocate and Session
      val sessionId = sessionid.untagged

      // Login manager request
      val loginRequest = req("enterprise", "/check",
        ("username" -> "foo"), ("password" -> "bar"))

      // Original request
      val origReq = req("enterprise", "/ent", ("fake" -> "drake"))
      sessionStore.update[Request](Session(sessionId, origReq))

      // Execute
      val output = keymasterIdentityProviderChain(sessionStore).apply(
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
      val sessionId = sessionid.untagged

      // Login manager request
      val loginRequest = req("enterprise", "/loginConfirm",
        ("username" -> "foo"), ("password" -> "bar"))

      // Original request
      val origReq = req("enterprise", "/ent", ("fake" -> "drake"))
      sessionStore.update[Request](Session(sessionId, origReq))

      // Execute
      val output = keymasterIdentityProviderChain(sessionStore).apply(
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
      val sessionId = sessionid.untagged

      // Original request
      val origReq = req("enterprise", "/ent")
      sessionStore.update[Tokens](Session(sessionId, tokens))

      // Execute
      val output = keymasterAccessIssuerChain(sessionStore).apply(
        SessionIdRequest(ServiceRequest(origReq, one), sessionId))

      // Validate
      Await.result(output).status should be(Status.Ok)
    } finally {
      server.close()
    }
  }
}
