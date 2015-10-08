package com.lookout.borderpatrol.auth.keymaster

import com.lookout.borderpatrol.auth.keymaster.Keymaster._
import com.lookout.borderpatrol.auth._
import com.lookout.borderpatrol.sessionx.SessionStores.MemcachedStore
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.{ServiceMatcher, ServiceIdentifier}
import com.lookout.borderpatrol.test._
import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.finagle.memcached.GetResult
import com.twitter.finagle.{memcached, Service}
import com.twitter.finagle.httpx.path.Path
import com.twitter.finagle.httpx.{Request, Status, Response}
import com.twitter.util.{Await, Future}
import io.circe._


class KeymasterSpec extends BorderPatrolSuite  {
  import sessionx.helpers.{secretStore => store, _}
  import Tokens._

  // sids
  val one = ServiceIdentifier("one", Path("/ent"), "enterprise", "/a/login")
  val serviceMatcher = ServiceMatcher(Set(one))
  val sessionStore = SessionStores.InMemoryStore

  // Request helper
  def req(subdomain: String, path: String, params: Tuple2[String, String]*): Request =
    Request(s"http://${subdomain + "."}example.com${path.toString}", params:_*)

  //  Tokens
  val serviceToken1 = new ServiceToken("SomeServiceTokenData1")
  val serviceToken2 = new ServiceToken("SomeServiceTokenData2")
  val serviceTokens = new ServiceTokens().add("service1", serviceToken1)
  val serviceTokens2 = serviceTokens.add("service2", serviceToken2)

  val tokens = new Tokens(new MasterToken("masterT"), serviceTokens)
  val tokens2 = tokens.add("one", serviceToken2)

  // Endpoint path
  val endpoint = "http://localhost/api/auth/service/v1/account_master_token"

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

  val keymasterLoginFilterTestService = new Service[IdentifyRequest[Credential], IdentifyResponse[Tokens]] {
    def apply(request: IdentifyRequest[Credential]) = Future(new KeymasterIdentifyRes(tokens))
  }

  val keymasterTestService = new Service[Request, Response] {
    def apply(request: Request) = Response(Status.Ok).toFuture
  }

  behavior of "KeymasterIdentityProvider"

  it should "Exception raised by api method in KeymasterIdentityProvider is propagated back" in {
    // Execute
    // Validate
    val caught = the [java.net.MalformedURLException] thrownBy {
      val output = new KeymasterIdentityProvider(keymasterTestService, "/api/endpoint")(new KeymasterIdentifyReq(new Credential("foo", "bar", one)))
    }
    caught.getMessage should equal ("no protocol: /api/endpoint")
  }

  it should "Error response from Keymaster service, KeymasterIdentityProvider raises a parsing exception" in {
    val testService = new Service[Request, Response] {
      def apply(request: Request) = Response(Status.NotAcceptable).toFuture
    }

    // Execute
    val output = new KeymasterIdentityProvider(testService, endpoint)(new KeymasterIdentifyReq(new Credential("foo", "bar", one)))

    // Validate
    //***FIXME: need to revisit this logic
    val caught = the [ParsingFailure] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("exhausted input")
  }

  it should "Invalid response content from Keymaster service, KeymasterIdentityProvider raises a parsing exception" in {
    val testService = new Service[Request, Response] {
      def apply(request: Request) =
        tap(Response(Status.Ok))(res => {
          res.contentString = "invalid string"; res.contentType = "application/json"
        }).toFuture
    }
    val endpoint = "http://localhost/api/auth/service/v1/account_master_token"

    // Execute
    val output = new KeymasterIdentityProvider(testService, endpoint)(new KeymasterIdentifyReq(new Credential("foo", "bar", one)))

    // Validate
    val caught = the [ParsingFailure] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("expected json value got i (line 1, column 1)")
  }

  it should "On successful Keymaster response, KeymasterIdentityProvider returns the IdentityResponse with tokens" in {
    val testService = new Service[Request, Response] {
      def apply(request: Request) =
        tap(Response(Status.Ok))(res => {
          res.contentString = TokensEncoder(tokens).toString(); res.contentType = "application/json"
        }).toFuture
    }
    val endpoint = "http://localhost/api/auth/service/v1/account_master_token"

    // Execute
    val output = new KeymasterIdentityProvider(testService, endpoint)(new KeymasterIdentifyReq(new Credential("foo", "bar", one)))

    // Validate
    Await.result(output) shouldBe a [KeymasterIdentifyRes]
    Await.result(output).identity should be (Id(tokens))
  }

  behavior of "KeymasterLoginFilter"

  it should "Missing credentials in the request, should send a 400 back" in {
    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Login POST request
    val loginRequest = req("enterprise", "/login", ("username" -> "foo"))
    loginRequest.addCookie(cooki)

    // Execute
    val output = (new KeymasterLoginFilter(sessionStore) andThen keymasterLoginFilterTestService)(new ServiceRequest(loginRequest, one))

    // Validate
    Await.result(output).status should be (Status.BadRequest)
  }

  it should "Missing sessionId in the request, throws a SessionIdError" in {
    // Login POST request
    val loginRequest = req("enterprise", "/login", ("username" -> "foo"), ("password" -> "bar"))

    // Execute
    val output = (new KeymasterLoginFilter(sessionStore) andThen keymasterLoginFilterTestService)(new ServiceRequest(loginRequest, one))

    // Validate
    val caught = the [SessionIdError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("An error occurred reading SessionId: no border_session cookie")
  }

  it should "Failure to find original request from sessionStore, returns " in {
    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Login POST request
    val loginRequest = req("enterprise", "/login", ("username" -> "foo"), ("password" -> "bar"))
    loginRequest.addCookie(cooki)

    // Execute
    val output = (new KeymasterLoginFilter(sessionStore) andThen keymasterLoginFilterTestService)(new ServiceRequest(loginRequest, one))

    // Validate
    val caught = the [SessionStoreError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should be equals ("An error occurred interacting with the session store: failed for SessionId")
  }

  it should "Exception throw by original request lookup from sessionStore, returns " in {
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
    val cooki = sessionId.asCookie

    // Login POST request
    val loginRequest = req("enterprise", "/login", ("username" -> "foo"), ("password" -> "bar"))
    loginRequest.addCookie(cooki)

    // Execute
    val output = (new KeymasterLoginFilter(mockSessionStore) andThen keymasterLoginFilterTestService)(new ServiceRequest(loginRequest, one))

    // Validate
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("oopsie")
  }

  it should "On a successful validation, this filter should send redirect with tokens" in {
    val service = new Service[IdentifyRequest[Credential], IdentifyResponse[Tokens]] {
      def apply(request: IdentifyRequest[Credential]) = {
        Future(new KeymasterIdentifyRes(tokens))
      }
    }

    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Login POST request
    val loginRequest = req("enterprise", "/login", ("username" -> "foo"), ("password" -> "bar"))
    loginRequest.addCookie(cooki)

    // Original request
    val origReq = req("enterprise", "/dang", ("fake" -> "drake"))
    sessionStore.update[Request](Session(sessionId, origReq))

    // Execute
    val output = (new KeymasterLoginFilter(sessionStore) andThen service)(new ServiceRequest(loginRequest, one))

    // Validate
    Await.result(output).status should be (Status.TemporaryRedirect)
    Await.result(output).location should be equals ("/dang")
    val returnedSessionId = SessionId.fromResponse(Await.result(output)).toFuture
    returnedSessionId should not be sessionId
    val session_d = sessionStore.get[Tokens](Await.result(returnedSessionId))
    val tokensz = sessionDataFromResponse(Await.result(output))
    Await.result(tokensz) should be (tokens)
  }

  behavior of "KeymasterAccessIssuer"

  it should "Exception raised by api method in KeymasterAccessIssuer is propagated back" in {
    val sessionId = sessionid.next

    // Execute
    // Validate
    val caught = the [java.net.MalformedURLException] thrownBy {
      val output = new KeymasterAccessIssuer(keymasterTestService, "/api/endpoint", sessionStore)(new KeymasterAccessReq(Id(tokens), one, sessionId))
    }
    caught.getMessage should equal ("no protocol: /api/endpoint")
  }

  it should "Error response from Keymaster service, KeymasterAccessIssuer raises a parsing exception" in {
    val testService = new Service[Request, Response] {
      def apply(request: Request) = Response(Status.NotAcceptable).toFuture
    }
    val sessionId = sessionid.next

    // Execute
    val output = new KeymasterAccessIssuer(testService, endpoint, sessionStore)(new KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    //***FIXME: need to revisit this logic
    val caught = the [ParsingFailure] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("exhausted input")
  }

  it should "Invalid response content from Keymaster service, KeymasterAccessIssuer raises a parsing exception" in {
    val testService = new Service[Request, Response] {
      def apply(request: Request) =
        tap(Response(Status.Ok))(res => {
          res.contentString = "invalid string"; res.contentType = "application/json"
        }).toFuture
    }
    val sessionId = sessionid.next

    // Execute
    val output = new KeymasterAccessIssuer(testService, endpoint, sessionStore)(new KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    val caught = the [ParsingFailure] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("expected json value got i (line 1, column 1)")
  }


  it should "If token for serviceId - one - is missing from response, KeymasterAccessIssuer raises AccessDenied error" in {
    val testService = new Service[Request, Response] {
      def apply(request: Request) =
        tap(Response(Status.Ok))(res => {
          res.contentString = TokensEncoder(tokens).toString();
          res.contentType = "application/json"
        }).toFuture
    }
    val sessionId = sessionid.next

    // Execute
    val output = new KeymasterAccessIssuer(testService, endpoint, sessionStore)(new KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    val caught = the [AccessDenied.type] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("no access available for this service")
  }

  it should "On successful Keymaster response, KeymasterAccessIssuer returns the AccessResponse with tokens" in {
    val testService = new Service[Request, Response] {
      def apply(request: Request) = {
        tap(Response(Status.Ok))(res => {
          res.contentString = TokensEncoder(tokens2).toString();
          res.contentType = "application/json"
        }).toFuture
      }
    }
    val sessionId = sessionid.next

    // Execute
    val output = new KeymasterAccessIssuer(testService, endpoint, sessionStore)(new KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    Await.result(output) shouldBe a [KeymasterAccessRes]
    Await.result(output).access.access should be (serviceToken2)
    val tokIt = getTokensFromSessionId(sessionId)
    Await.result(tokIt) should be (tokens2)
  }

  it should "KeymasterAccessIssuer uses service token from the cache" in {
    val testService = new Service[Request, Response] {
      def apply(request: Request) = {
        assert(false) // Should not get here
        Response(Status.Ok).toFuture
      }
    }
    val sessionId = sessionid.next

    // Execute
    val output = new KeymasterAccessIssuer(testService, endpoint, sessionStore)(new KeymasterAccessReq(Id(tokens2), one, sessionId))

    // Validate
    Await.result(output) shouldBe a [KeymasterAccessRes]
    Await.result(output).access.access should be (serviceToken2)
  }
}
