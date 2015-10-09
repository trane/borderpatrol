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
  val path = Path("/api/auth/service/v1/account_master_token")

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
    req => Future(new KeymasterIdentifyRes(tokens)) }
  val keymasterTestService = mkTestService[Request, Response] { req => Response(Status.Ok).toFuture }

  behavior of "KeymasterIdentityProvider"

  it should "succeed and return IdentityResponse with tokens received from upstream Keymaster Service" in {
    val testService = mkTestService[Request, Response] {
      request => tap(Response(Status.Ok))(res => {
        res.contentString = TokensEncoder(tokens).toString()
        res.contentType = "application/json"
      }).toFuture
    }

    // Execute
    val output = new KeymasterIdentityProvider(testService, path)(new KeymasterIdentifyReq(new Credential("foo", "bar", one)))

    // Validate
    Await.result(output).identity should be (Id(tokens))
  }

  it should "propagate the error Status code from Keymaster service in the AccessDenied exception" in {
    val testService = mkTestService[Request, Response] { request => Response(Status.NotFound).toFuture }

    // Execute
    val output = new KeymasterIdentityProvider(testService, path)(new KeymasterIdentifyReq(new Credential("foo", "bar", one)))

    // Validate
    val caught = the [AccessDenied] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("Invalid credentials for user foo")
    caught.status should be (Status.NotFound)
  }

  it should "propagate the failure parsing the response from Keymaster service as an AccessDenied exception" in {
    val testService = mkTestService[Request, Response] {
      request => tap(Response(Status.Ok))(res => {
        res.contentString = "invalid string"
        res.contentType = "application/json"
      }).toFuture
    }

    // Execute
    val output = new KeymasterIdentityProvider(testService, path)(new KeymasterIdentifyReq(new Credential("foo", "bar", one)))

    // Validate
    val caught = the [AccessDenied] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("Failed to parse the Keymaster Identity Response")
    caught.status should be (Status.InternalServerError)
  }

  behavior of "KeymasterLoginFilter"

  it should "succeed and saves tokens, sends redirect with tokens returned by IDP" in {
    val testService = mkTestService[IdentifyRequest[Credential], IdentifyResponse[Tokens]] {
      request => Future(new KeymasterIdentifyRes(tokens))
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
    val output = (new KeymasterLoginFilter(sessionStore) andThen testService)(new ServiceRequest(loginRequest, one))

    // Validate
    Await.result(output).status should be (Status.TemporaryRedirect)
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
    val cooki = sessionId.asCookie

    // Login POST request
    val loginRequest = req("enterprise", "/login", ("username" -> "foo"))
    loginRequest.addCookie(cooki)

    // Execute
    val output = (new KeymasterLoginFilter(sessionStore) andThen keymasterLoginFilterTestService)(new ServiceRequest(loginRequest, one))

    // Validate
    Await.result(output).status should be (Status.BadRequest)
  }

  it should "throw a SessionIdError, if sessionId is missing from the Request" in {
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

  it should "return SessionStoreError if it fails find the original request from sessionStore" in {
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

  behavior of "KeymasterAccessIssuer"

  it should "succeed, return service token found in the ServiceTokens cache" in {
    val testService = mkTestService[Request, Response] {
      request => { assert(false); Response(Status.Ok).toFuture }
    }
    val sessionId = sessionid.next

    // Execute
    val output = new KeymasterAccessIssuer(testService, path, sessionStore)(new KeymasterAccessReq(Id(tokens2), one, sessionId))

    // Validate
    Await.result(output).access.access should be (serviceToken2)
  }

  it should "succeed, save in SessionStore and return the ServiceToken received from the Keymaster Service" in {
    val testService = mkTestService[Request, Response] {
      request => tap(Response(Status.Ok))(res => {
        res.contentString = TokensEncoder(tokens2).toString()
        res.contentType = "application/json"
      }).toFuture
    }
    val sessionId = sessionid.next

    // Execute
    val output = new KeymasterAccessIssuer(testService, path, sessionStore)(new KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    Await.result(output).access.access should be (serviceToken2)
    val tokIt = getTokensFromSessionId(sessionId)
    Await.result(tokIt) should be (tokens2)
  }

  it should "propagate the error Status code returned by the Keymaster service, as the AccessDenied exception" in {
    val testService = mkTestService[Request, Response] { request => Response(Status.NotFound).toFuture }
    val sessionId = sessionid.next

    // Execute
    val output = new KeymasterAccessIssuer(testService, path, sessionStore)(new KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    val caught = the [AccessDenied] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("No access allowed to service one")
    caught.status should be (Status.NotFound)
  }

  it should "propagate the failure to parse response content from Keymaster service, as AccessDenied exception" in {
    val testService = mkTestService[Request, Response] {
      request => tap(Response(Status.Ok))(res => {
        res.contentString = "invalid string"
        res.contentType = "application/json"
      }).toFuture
    }
    val sessionId = sessionid.next

    // Execute
    val output = new KeymasterAccessIssuer(testService, path, sessionStore)(new KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    val caught = the [AccessDenied] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("Failed to parse the Keymaster Access Response")
  }

  it should "return an AccessDenied exception, if it fails to find the ServiceToken in the Keymaster response" in {
    val testService = mkTestService[Request, Response] {
      request => tap(Response(Status.Ok))(res => {
        res.contentString = TokensEncoder(tokens).toString()
        res.contentType = "application/json"
      }).toFuture
    }
    val sessionId = sessionid.next

    // Execute
    val output = new KeymasterAccessIssuer(testService, path, sessionStore)(new KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    val caught = the [AccessDenied] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("No access allowed to service one")
    caught.status should be (Status.NotAcceptable)
  }
}
