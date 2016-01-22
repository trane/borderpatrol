package com.lookout.borderpatrol.test.security

import com.lookout.borderpatrol.security.Csrf._
import com.lookout.borderpatrol.security.{CsrfInsertFilter, CsrfVerifyFilter}
import com.lookout.borderpatrol.test._
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Response, Cookie, Request, Status}
import com.twitter.util.{Await, Future}

class CsrfSpec extends BorderPatrolSuite {
  import sessionx.helpers.{secretStore => store, _}

  val csrf1 = sessionid.untagged.asBase64
  val csrf2 = sessionid.untagged.asBase64

  val (header, param, cookiename, verified) = ("header", "param", "cookieName", "verified")
  val verify = Verify(InHeader(header), Param(param), CookieName(cookiename), VerifiedHeader(verified))

  val filter = CsrfVerifyFilter(verify)

  val service = Service.mk[Request, Response]{ req =>
    Future.value {
      req.headerMap.get(verified) match {
        case Some("true") => Response(Status.Ok)
        case Some("false") => Response(Status.Forbidden)
        case _ => throw new Exception("bottom")
      }
    }
  }

  val testOkService = Service.mk[Request, Response]{ req => Future.value(Response(Status.Ok)) }

  def requestWithHeader: Request = {
    val req = Request("/")
    req.addCookie(new Cookie(cookiename, csrf1))
    req.headerMap.add(header, csrf1)
    req
  }

  def requestWithParam: Request = {
    val req = Request("/", param -> csrf1)
    req.addCookie(new Cookie(cookiename, csrf1))
    req
  }

  behavior of "Verify"

  it should "return a request with a verified header set" in {
    val req = Request("/")
    verify.unsafeInject(req)(_.toString)

    req.headerMap.get(verified) should not be(None)
    req.headerMap.get(verified).value should be(false.toString)
  }

  it should "set verified header as true when Csrf header or param is set correctly" in {
    val req1 = requestWithHeader
    val req2 = requestWithParam

    verify.unsafeInject(req1)(_.toString)
    verify.unsafeInject(req2)(_.toString)

    req1.headerMap.get(verified) should not be(None)
    req2.headerMap.get(verified) should not be(None)
    req1.headerMap.get(verified).value should be(true.toString)
    req2.headerMap.get(verified).value should be(true.toString)
  }

  it should "set verified header as false when Csrf header or param is set incorrectly" in {
    val req1 = requestWithHeader
    val req2 = Request("/", param -> csrf2)
    req1.headerMap.update(header, csrf2)
    req2.addCookie(new Cookie(cookiename, csrf1))

    verify.unsafeInject(req1)(_.toString)
    verify.unsafeInject(req2)(_.toString)

    req1.headerMap.get(verified) should not be(None)
    req2.headerMap.get(verified) should not be(None)
    req1.headerMap.get(verified).value should be(false.toString)
    req2.headerMap.get(verified).value should be(false.toString)
  }

  it should "set verified header as false when Csrf cookie is set incorrectly" in {
    val req1 = requestWithHeader
    val req2 = requestWithParam
    req1.cookies.remove(cookiename)
    req2.cookies.update(cookiename, new Cookie(cookiename, csrf2))

    verify.unsafeInject(req1)(_.toString)
    verify.unsafeInject(req2)(_.toString)

    req1.headerMap.get(verified) should not be(None)
    req2.headerMap.get(verified) should not be(None)
    req1.headerMap.get(verified).value should be(false.toString)
    req2.headerMap.get(verified).value should be(false.toString)
  }

  behavior of "CsrfVerifyFilter"

  it should "make an Ok response when verify sets header to true" in {
    filter(requestWithParam, service).results.status should be(Status.Ok)
  }

  it should "make a Forbidden response when verify sets header to false" in {
    filter(Request("/"), service).results.status should be(Status.Forbidden)
  }

  it should "override any verified header set in incoming request" in {
    val req = Request("/")
    req.headerMap.add(verified, true.toString)
    filter(req, service).results.status should be(Status.Forbidden)
  }

  behavior of "CsrfInsertFilter"

  it should "make an Ok response when verify sets header to true" in {
    val req = Request("/")
    val output = (CsrfInsertFilter[Request](CookieName(cookiename)) andThen testOkService)(req)

    Await.result(output).status should be (Status.Ok)
    Await.result(output).cookies.get(cookiename) should not be (None)
  }
}
