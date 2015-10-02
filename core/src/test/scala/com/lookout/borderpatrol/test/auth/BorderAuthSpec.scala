package com.lookout.borderpatrol.auth

import com.lookout.borderpatrol.sessionx.SessionStore.MemcachedStore
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
import _root_.java.lang.{Boolean => JBoolean}


class BorderAuthSpec extends BorderPatrolSuite  {
  import sessionx.helpers.{secretStore => store, _}

  // sids
  val one = ServiceIdentifier("one", Path("/ent"), "enterprise", "/a/login")
  val serviceMatcher = ServiceMatcher(Set(one))
  val sessionStore = SessionStore.InMemoryStore

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
  val identityFilterTestService = new Service[AccessRequest[Int], Response] {
    def apply(request: AccessRequest[Int]) = {
      Future.value(Response(Status.Ok))
    }
  }

  //  Mock SessionStore client
  case object FailingMockClient extends memcached.MockClient {
    override def add(key: String, flags: Int, expiry: Time, value: Buf): Future[JBoolean] =
      Future.exception[JBoolean](new Exception("oopsie"))
    override def getResult(keys: Iterable[String]): Future[GetResult] =
      Future.exception(new Exception("oopsie"))
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

  behavior of "IdentifyFilter"

  it should "Lack of SessionId in the ServiceRequest returns a Redirect to login URI" in {
    // Create request
    val request = req("enterprise", "/dang")

    // Execute
    val output = (new IdentityFilter[Int](sessionStore) andThen identityFilterTestService)(
      new ServiceRequest(request, one))

    // Validate
    Await.result(output).status should be (Status.TemporaryRedirect)
    Await.result(output).location should be equals(one.login)
    val sessionData = sessionDataFromResponse(Await.result(output))
    Await.result(sessionData).path should be equals(request.path)
  }

  it should "Failure to find Session by SessionId returns a Redirect to login URI" in {
    val service = new Service[AccessRequest[Request], Response] {
      def apply(request: AccessRequest[Request]) = Future.value(Response(Status.Ok))
    }

    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Create request
    val request = req("enterprise", "/dang")
    request.addCookie(cooki)

    // Execute
    val output = (new IdentityFilter[Request](sessionStore) andThen service)(new ServiceRequest(request, one))

    // Verify
    Await.result(output).status should be (Status.TemporaryRedirect)
    Await.result(output).location should be equals(one.login)
    val returnedSessionId = SessionId.fromResponse(Await.result(output)).toFuture
    Await.result(returnedSessionId) should not equals(sessionId)
    val sessionData = sessionDataFromResponse(Await.result(output))
    Await.result(sessionData).path should be equals(request.path)
  }

  it should "SessionStore.get throws an exception returns Redirect to login" in {
    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Create request
    val request = req("enterprise", "/dang")
    request.addCookie(cooki)

    // Mock sessionStore
    val mockSessionStore = MemcachedStore(FailingMockClient)

    // Execute
    val output = (new IdentityFilter[Int](mockSessionStore) andThen identityFilterTestService)(
      new ServiceRequest(request, one))

    // Verify
    Await.result(output).status should be (Status.TemporaryRedirect)
    Await.result(output).location should be equals(one.login)
    SessionId.fromResponse(Await.result(output)) should not be null
  }

  it should "Exception thrown by SessionStore.update is propogated" in {
    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Mock sessionStore
    val mockSessionStore = MemcachedStore(FailingMockClient)

    // Create request
    val request = req("enterprise", "/dang")
    request.addCookie(cooki)

    // Execute
    val output = (new IdentityFilter[Int](mockSessionStore) andThen identityFilterTestService)(new ServiceRequest(request, one))

    // Verify
    //***FIXME: Verify that output is Throwable i.e. Exception("oopsie") and it does not generate a redirect. but generates a 500 back
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
    val output = (new IdentityFilter[Int](sessionStore) andThen service)(
      new ServiceRequest(request, one))

    //  Verify
    Await.result(output).status should be (Status.Ok)
  }
}