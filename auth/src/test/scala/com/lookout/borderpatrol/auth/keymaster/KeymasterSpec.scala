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
import com.twitter.finagle.httpx.{Method, Request, Status, Response}
import com.twitter.util.{Await, Future}


class KeymasterSpec extends BorderPatrolSuite  {
  import sessionx.helpers.{secretStore => store, _}
  import Tokens._

  // sids
  val one = ServiceIdentifier("one", Path("/ent"), "enterprise", Path("/a/login"))
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
    req => Future(KeymasterIdentifyRes(tokens)) }
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
    val output = KeymasterIdentityProvider(testService, path)(KeymasterIdentifyReq(Credential("foo", "bar", one)))

    // Validate
    Await.result(output).identity should be (Id(tokens))
  }

  it should "propagate the error Status code from Keymaster service in the IdentityProviderError exception" in {
    val testService = mkTestService[Request, Response] { request => Response(Status.NotFound).toFuture }

    // Execute
    val output = KeymasterIdentityProvider(testService, path)(KeymasterIdentifyReq(Credential("foo", "bar", one)))

    // Validate
    val caught = the [IdentityProviderError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("Invalid credentials for user foo")
    caught.status should be (Status.NotFound)
  }

  it should "propagate the failure parsing the response from Keymaster service as an IdentityProviderError exception" in {
    val testService = mkTestService[Request, Response] {
      request => tap(Response(Status.Ok))(res => {
        res.contentString = """{"key":"data"}"""
        res.contentType = "application/json"
      }).toFuture
    }

    // Execute
    val output = KeymasterIdentityProvider(testService, path)(KeymasterIdentifyReq(Credential("foo", "bar", one)))

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
    val testService = mkTestService[Request, Response] {
      request => { assert(false); Response(Status.Ok).toFuture }
    }
    val sessionId = sessionid.next

    // Execute
    val output = KeymasterAccessIssuer(testService, path, sessionStore)(KeymasterAccessReq(Id(tokens2), one, sessionId))

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
    sessionStore.update[Tokens](Session(sessionId, tokens))

    // Execute
    val output = KeymasterAccessIssuer(testService, path, sessionStore)(KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    Await.result(output).access.access should be (serviceToken2)
    val tokIt = getTokensFromSessionId(sessionId)
    Await.result(tokIt) should be (tokens2)
  }

  it should "propagate the error Status code returned by the Keymaster service, as the AccessIssuerError exception" in {
    val testService = mkTestService[Request, Response] { request => Response(Status.NotFound).toFuture }
    val sessionId = sessionid.next

    // Execute
    val output = KeymasterAccessIssuer(testService, path, sessionStore)(KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    val caught = the [AccessIssuerError] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("No access allowed to service one due to error: Status(404)")
    caught.status should be (Status.NotFound)
  }

  it should "propagate the failure to parse response content from Keymaster service, as AccessIssuerError exception" in {
    val testService = mkTestService[Request, Response] {
      request => tap(Response(Status.Ok))(res => {
        res.contentString = "invalid string"
        res.contentType = "application/json"
      }).toFuture
    }
    val sessionId = sessionid.next

    // Execute
    val output = KeymasterAccessIssuer(testService, path, sessionStore)(KeymasterAccessReq(Id(tokens), one, sessionId))

    // Validate
    val caught = the [AccessIssuerError] thrownBy {
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
    val output = KeymasterAccessIssuer(testService, path, sessionStore)(KeymasterAccessReq(Id(tokens), one, sessionId))

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
    val upstreamService = mkTestService[Request, Response] {
      request => {
        // Verify service token in the request
        assert (request.headerMap.get("Auth-Token") == Some(serviceToken2.value))
        Response(Status.Ok).toFuture
      }
    }

    // Allocate and Session
    val sessionId = sessionid.next

    // Create request
    val request = req("enterprise", "/dang")

    // Execute
    val output = (KeymasterAccessFilter(upstreamService) andThen accessService)(
      AccessIdRequest(SessionIdRequest(ServiceRequest(request, one), sessionId), Id(tokens)))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "propagate the failure status code returned by upstream service" in {
    val accessService = mkTestService[AccessRequest[Tokens], AccessResponse[ServiceToken]] {
      request => KeymasterAccessRes(Access(serviceToken2)).toFuture
    }
    val upstreamService = mkTestService[Request, Response] {
      request => {
        // Verify service token in the request
        assert (request.headerMap.get("Auth-Token") == Some(serviceToken2.value))
        Response(Status.NotFound).toFuture
      }
    }

    // Allocate and Session
    val sessionId = sessionid.next

    // Create request
    val request = req("enterprise", "/dang")

    // Execute
    val output = (KeymasterAccessFilter(upstreamService) andThen accessService)(
      AccessIdRequest(SessionIdRequest(ServiceRequest(request, one), sessionId), Id(tokens)))

    // Validate
    Await.result(output).status should be (Status.NotFound)
  }

  it should "propagate the exception returned by Access Issuer Service" in {
    val accessService = mkTestService[AccessRequest[Tokens], AccessResponse[ServiceToken]] {
      request => Future.exception(new Exception("Oopsie"))
    }
    val upstreamService = mkTestService[Request, Response] {
      request => {
        // Verify service token in the request
        assert (request.headerMap.get("Auth-Token") == Some(serviceToken2.value))
        Response(Status.NotFound).toFuture
      }
    }

    // Allocate and Session
    val sessionId = sessionid.next

    // Create request
    val request = req("enterprise", "/dang")

    // Execute
    val output = (KeymasterAccessFilter(upstreamService) andThen accessService)(
      AccessIdRequest(SessionIdRequest(ServiceRequest(request, one), sessionId), Id(tokens)))

    // Validate
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
  }

  behavior of "KeymasterMethodMuxLoginFilter"

  it should "succeed and invoke the GET on keymaster service" in {
    val testService = mkTestService[SessionIdRequest, Response] { _ => fail("Should not get here") }
    val keymasterService = mkTestService[Request, Response] { _ => Response(Status.Ok).toFuture }

    // Allocate and Session
    val sessionId = sessionid.next

    // Create request
    val request = Request(Method.Get, "/ent")

    // Execute
    val output = (KeymasterMethodMuxLoginFilter(keymasterService, path) andThen testService)(
      SessionIdRequest(ServiceRequest(request, one), sessionId))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "succeed and invoke the POST on upstream service" in {
    val testService = mkTestService[SessionIdRequest, Response] { _ => Response(Status.Ok).toFuture }
    val keymasterService = mkTestService[Request, Response] { _ => fail("Should not get here") }

    // Allocate and Session
    val sessionId = sessionid.next

    // Create request
    val request = Request(Method.Post, "/ent")

    // Execute
    val output = (KeymasterMethodMuxLoginFilter(keymasterService, path) andThen testService)(
      SessionIdRequest(ServiceRequest(request, one), sessionId))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "succeed and invoke the non GET or POST method on keymaster service" in {
    val testService = mkTestService[SessionIdRequest, Response] { _ => fail("Should not get here") }
    val keymasterService = mkTestService[Request, Response] { _ => Response(Status.Ok).toFuture }

    // Allocate and Session
    val sessionId = sessionid.next

    // Create request
    val request = Request(Method.Head, "/ent")

    // Execute
    val output = (KeymasterMethodMuxLoginFilter(keymasterService, path) andThen testService)(
      SessionIdRequest(ServiceRequest(request, one), sessionId))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }
}
