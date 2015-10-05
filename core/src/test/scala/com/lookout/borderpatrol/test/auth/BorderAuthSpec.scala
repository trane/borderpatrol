package com.lookout.borderpatrol.auth

import com.lookout.borderpatrol.sessionx.SessionStores.MemcachedStore
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.test._
import com.lookout.borderpatrol.{ServiceMatcher, ServiceIdentifier}
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

  // sids
  val one = ServiceIdentifier("one", Path("/ent"), "enterprise", "/a/login")
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
  val serviceFilterTestService = new Service[ServiceRequest, Response] {
    def apply(request: ServiceRequest) = Future.value(Response(Status.Ok))
  }
  val sessionIdFilterTestService = new Service[SessionIdRequest, Response] {
    def apply(request: SessionIdRequest) = {
      println(request); Future.value(Response(Status.Ok))
    }
  }
  val identityFilterTestService = new Service[AccessRequest[Request], Response] {
    def apply(request: AccessRequest[Request]) = Future.value(Response(Status.Ok))
  }

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

  it should "find the valid Service by match" in {
    // Execute
    val output = (new ServiceFilter(serviceMatcher) andThen serviceFilterTestService)(req("enterprise", "/dang"))

    // Validate
    Await.result(output).status should be (Status.Ok)
  }

  it should "return NotFound if Service not found by path or subdomain match" in {
    // Execute
    val output = (new ServiceFilter(serviceMatcher) andThen serviceFilterTestService)(req("foo", "/bar"))

    // Validate
    Await.result(output).status should be (Status.NotFound)
  }

  behavior of "SessionIdFilter"

  it should "Lack of SessionId in the SessionIdRequest returns a Redirect to login URI" in {

    // Create request
    val request = req("enterprise", "/dang")

    // Execute
    val output = (new SessionIdFilter(sessionStore) andThen sessionIdFilterTestService)(new ServiceRequest(request, one))

    // Validate
    Await.result(output).status should be (Status.TemporaryRedirect)
    Await.result(output).location should be equals(one.login)
    val sessionData = sessionDataFromResponse(Await.result(output))
    Await.result(sessionData).path should be equals(request.path)
  }

  it should "Failure returned by the Service, is propogated back as-is" in {
    val service = new Service[SessionIdRequest, Response] {
      def apply(request: SessionIdRequest) = Future.value(Response(Status.NotFound))
    }

    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Create request
    val request = req("enterprise", "/dang")
    request.addCookie(cooki)

    // Execute
    val output = (new SessionIdFilter(sessionStore) andThen service)(new ServiceRequest(request, one))

    // Verify
    Await.result(output).status should be (Status.NotFound)
  }

  it should "Exception returned by the Service, is propogated back as-is" in {
    val service = new Service[SessionIdRequest, Response] {
      def apply(request: SessionIdRequest) = Future.exception(new Exception("SessionIdFilter test failure"))
    }

    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Create request
    val request = req("enterprise", "/dang")
    request.addCookie(cooki)

    // Execute
    val output = (new SessionIdFilter(sessionStore) andThen service)(new ServiceRequest(request, one))

    // Verify
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("SessionIdFilter test failure")
  }

  it should "Exception thrown by SessionStore.update, is propogated as-is" in {
    // Mock sessionStore
    val mockSessionStore = MemcachedStore(FailingMockClient)

    // Create request
    val request = req("enterprise", "/dang")

    // Execute
    val output = (new SessionIdFilter(mockSessionStore) andThen sessionIdFilterTestService)(new ServiceRequest(request, one))

    // Verify
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("oopsie")
  }

  it should "For a ServiceRequest with SessionId, a Service response of 200 OK is propogated back" in {
    val service = new Service[SessionIdRequest, Response] {
      def apply(request: SessionIdRequest) = {
        assert(request.req.req.path == "/dang")
        assert(request.req.id == one)
        Future.value(Response(Status.Ok))
      }
    }

    //  Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    //  Create request
    val request = req("enterprise", "/dang")
    request.addCookie(cooki)

    //  Execute
    val output = (new SessionIdFilter(sessionStore) andThen service)(new ServiceRequest(request, one))

    //  Verify
    Await.result(output).status should be (Status.Ok)
  }

  behavior of "IdentityFilter"

  it should "Failure to find Session by SessionId returns a Redirect to login URI" in {

    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Create request
    val request = req("enterprise", "/dang")
    request.addCookie(cooki)

    // Execute
    val output = (new IdentityFilter[Request](sessionStore) andThen identityFilterTestService)(SessionIdRequest(ServiceRequest(request, one), sessionId))

    // Verify
    Await.result(output).status should be (Status.TemporaryRedirect)
    Await.result(output).location should be equals(one.login)
    val returnedSessionId = SessionId.fromResponse(Await.result(output)).toFuture
    Await.result(returnedSessionId) should not equals(sessionId)
    val sessionData = sessionDataFromResponse(Await.result(output))
    Await.result(sessionData).path should be equals(request.path)
  }

  it should "Exception thrown by SessionStore.get, is propogated as-is" in {
    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Create request
    val request = req("enterprise", "/dang")
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

  it should "Exception thrown by SessionStore.update, is propogated as-is" in {
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
    val request = req("enterprise", "/dang")
    request.addCookie(cooki)

    // Execute
    val output = (new IdentityFilter[Request](mockSessionStore) andThen identityFilterTestService)(SessionIdRequest(ServiceRequest(request, one), sessionId))

    // Verify
    val caught = the [Exception] thrownBy {
      Await.result(output)
    }
    caught.getMessage should equal ("whoopsie")
  }

  it should "A ServiceRequest with SessionId available in SessionStore returns 200 OK" in {
    val service = new Service[AccessRequest[Int], Response] {
      def apply(request: AccessRequest[Int]) = {
        assert(request.identity == Identity(999))
        Future.value(Response(Status.Ok)) }
    }

    //  Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    //  Create request
    val request = req("enterprise", "/dang")
    request.addCookie(cooki)
    val sessionData = 999
    sessionStore.update[Int](Session(sessionId, sessionData))

    //  Execute
    val output = (new IdentityFilter[Int](sessionStore) andThen service)(SessionIdRequest(ServiceRequest(request, one), sessionId))

    //  Verify
    Await.result(output).status should be (Status.Ok)
  }
}