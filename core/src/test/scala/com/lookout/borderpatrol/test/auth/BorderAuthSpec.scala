package com.lookout.borderpatrol.auth

import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.test.sessionx.helpers.sessionid
import com.lookout.borderpatrol.{ServiceMatcher, ServiceIdentifier}
import com.lookout.borderpatrol.test.BorderPatrolSuite
import com.twitter.finagle.Service
import com.twitter.finagle.httpx._
import com.twitter.finagle.httpx.path.Path
import com.twitter.util.{Await, Future}
import org.scalatest.mock.MockitoSugar

import scala.{Some, Option}

class BorderAuthSpec extends BorderPatrolSuite with MockitoSugar {
  import com.lookout.borderpatrol.test.sessionx.helpers

  val one = ServiceIdentifier("one", Path("/ent"), "enterprise", "/a/login")
  val serviceMatcher = ServiceMatcher(Set(one))
  //implicit val secretStore = SecretStores.InMemorySecretStore(Secrets(Secret(), Secret()))
  val sessionStore = SessionStore.InMemoryStore

  def req(subdomain: String = "nothing", path: String = "/"): Request =
    RequestBuilder().url(s"http://${subdomain + "."}example.com${path.toString}").buildGet()

  def identityFromResponse(resp: Response): Future[Identity[Request]] =
    (for {
      sessionId <- SessionId.fromResponse(resp).toFuture
      sessionMaybe <- sessionStore.get[Request](sessionId)
    } yield sessionMaybe.fold[Identity[Request]](EmptyIdentity)(s => Id(s.data)))

  behavior of "ServiceFilter"

  it should "find the valid Service by match" in {
    val service = new Service[ServiceRequest, Response] {
      def apply(request: ServiceRequest) = Future.value(Response(Status.Ok))
    }
    val output = (new ServiceFilter(serviceMatcher) andThen service)(req("enterprise", "/dang"))
    Await.result(output).status should be (Status.Ok)
  }

  it should "return NotFound if Service not found by path or subdomain match" in {
    val service = new Service[ServiceRequest, Response] {
      def apply(request: ServiceRequest) = Future.value(Response(Status.Ok))
    }
    val output = (new ServiceFilter(serviceMatcher) andThen service)(req("foo", "/bar"))
    //Await.result(output).status should be (Status.NotFound)
    //output.results.status NOTFOund
    //output.isThrowable
  }

  behavior of "IdentifyFilter"

  it should "Lack of SessionId in the ServiceRequest returns a Redirect to login URI" in {
    val service = new Service[AccessRequest[Int], Response] {
      def apply(request: AccessRequest[Int]) = {
        //***FIXME: Verify "Id" contents in AccessRequest
        Future.value(Response(Status.Ok))
      }
    }

    // Create request
    val request = req("enterprise", "/dang")

    val output = (new IdentityFilter[Int](sessionStore) andThen service)(new ServiceRequest(request, one))
    Await.result(output).status should be (Status.TemporaryRedirect)
    Await.result(output).location should be equals("/a/login")
    val futIdentity = identityFromResponse(Await.result(output))
    println("futidentity: " + Await.result(futIdentity))

    //***FIXME: How to add check "should be of type Some(Cookie)"
    //println(cooki)

    //***FIXME: Get SessionId from cookie and verify it in the SessionStore
//    val sessionData = identity[Int](Await.result(output))
//    println(Await.result(sessionData))
//    for {
//      sessionId <- SessionId.fromResponse(Await.result(output))
//      sessionMaybe <- sessionStore.get[Int](sessionId)
//    } yield
//    val sessionId = cooki.map(_.value).asInstanceOf[SessionId]
//    println(sessionId)
//    val sessionData = sessionStore.get(sessionId)
//    println(sessionData)

//    cookie should not be null
//    sessionStore.get(cookie.exists()sessionId)
  }

  it should "Failure to find Session by SessionId returns a Redirect to login URI" in {
    val service = new Service[AccessRequest[Int], Response] {
      def apply(request: AccessRequest[Int]) = Future.value(Response(Status.Ok))
    }

    // Allocate and Session
    val cooki = sessionid.next.asCookie

    // Create request
    val request = req("enterprise", "/dang")
    request.addCookie(cooki)

    val output = (new IdentityFilter[Int](sessionStore) andThen service)(new ServiceRequest(request, one))
    Await.result(output).status should be (Status.TemporaryRedirect)
    Await.result(output).location should be equals("/a/login")

    val futIdentity = identityFromResponse(Await.result(output))
    //futIdentity.results should be

    val cooki2 = Await.result(output).cookies.get("border_session")
    println(cooki2)
    //***FIXME: Verify cookie is not null

    //***FIXME: Get SessionId from cookie and verify it in the SessionStore
    //***FIXME: Make sure thi sessionId is different than one sent earlier in the request
  }

  //***FIXME: Do we need to test this?
//  it should "SessionStore update throws an exception returns Redirect to login" in {
//    val service = new Service[AccessRequest[Int], Response] {
//      def apply(request: AccessRequest[Int]) = Future.value(Response(Status.Ok))
//    }
//
//    // Allocate and Session
//    val sessionId = nextSessionId
//    val cooki = encodeId[Cookie](sessionId)
//
//    // Create request
//    val request = req("enterprise", "/dang")
//    request.addCookie(cooki)
//
//    val output = (new IdentityFilter[Int](sessionStore) andThen service)(new ServiceRequest(request, one))
//    Await.result(output).status should be (Status.TemporaryRedirect)
//    Await.result(output).location should be equals("/a/login")
//    val cooki2 = Await.result(output).cookies.get("border_session")
//    println(cooki2)
//    //***FIXME: Verify cookie is not null
//
//    //***FIXME: Get SessionId from cookie and verify it in the SessionStore
//    //***FIXME: Make sure thi sessionId is different than one sent earlier in the request
//  }

  it should "A ServiceRequest with SessionId available in SessionStore returns 200 OK" in {
    val service = new Service[AccessRequest[Int], Response] {
      def apply(request: AccessRequest[Int]) = Future.value(Response(Status.Ok))
    }

    // Allocate and Session
    val sessionId = sessionid.next
    val cooki = sessionId.asCookie

    // Create request
    val request = req("enterprise", "/dang")
    request.addCookie(cooki)
    val sessionData = 999
    sessionStore.update[Int](Session(sessionId, sessionData))

    val output = (new IdentityFilter[Int](sessionStore) andThen service)(new ServiceRequest(request, one))
    Await.result(output).status should be (Status.Ok)
  }
}