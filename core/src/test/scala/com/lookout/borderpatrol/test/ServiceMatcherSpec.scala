package com.lookout.borderpatrol.test

import com.lookout.borderpatrol._
import com.twitter.finagle.http.path.Path

class ServiceMatcherSpec extends BorderPatrolSuite {
  import sessionx.helpers._

  val sOneOne = ServiceIdentifier("eOne", urls, Path("/ent1"), None)
  val sOneTwo = ServiceIdentifier("eTwo", urls, Path("/ent2"), None)
  val sTwo = ServiceIdentifier("two", urls, Path("/api"), None)
  val sThree = ServiceIdentifier("three", urls, Path("/apis"), None)
  val sFour = ServiceIdentifier("four", urls, Path("/apis/test"), None)
  val serviceIds = Set(sOneOne, sOneTwo, sTwo, sThree, sFour)
  val cOne = CustomerIdentifier("enterprise", sOneOne, checkpointLoginManager)
  val cTwo = CustomerIdentifier("api", two, umbrellaLoginManager)
  val cThree = CustomerIdentifier("api.subdomain", sThree, checkpointLoginManager)
  val cFour = CustomerIdentifier("api.testdomain", sFour, umbrellaLoginManager)
  val custIds = Set(cOne, cTwo, cThree, cFour)
  val testServiceMatcher = ServiceMatcher(custIds, serviceIds)

  def getWinner(cid: CustomerIdentifier, sid: ServiceIdentifier): (CustomerIdentifier, ServiceIdentifier) =
    testServiceMatcher.get(req(cid.subdomain, sid.path.toString)).value

  behavior of "ServiceMatchers"

  it should "match the longest path" in {
    testServiceMatcher.path(Path("/")) should be(None)
    testServiceMatcher.path(Path("/e")) should be(None)
    testServiceMatcher.path(Path("/ent")) should be(None)
    testServiceMatcher.path(Path("/ent1/blah")).value should be(sOneOne)
    testServiceMatcher.path(Path("/ent2")).value should be(sOneTwo)
    testServiceMatcher.path(Path("/api")).value should be(sTwo)
    testServiceMatcher.path(Path("/apis")).value should be(sThree)
    testServiceMatcher.path(Path("/apis/testing")).value should be(sThree)
    testServiceMatcher.path(Path("/apis/test")).value should be(sFour)
  }

  it should "match the longest subdomain" in {
    testServiceMatcher.subdomain("www.example.com") should be(None)
    testServiceMatcher.subdomain("enterprise.api.example.com").value should be(cOne)
    testServiceMatcher.subdomain("enterprise.example.com").value should be(cOne)
    testServiceMatcher.subdomain("api.example.com").value should be(cTwo)
    testServiceMatcher.subdomain("api.subdomains.example.com").value should be(cTwo)
    testServiceMatcher.subdomain("api.subdomain.example.com").value should be(cThree)
  }

  it should "match the longest get" in {
    testServiceMatcher.get(req("enterprise", "/")) should be(None)
    testServiceMatcher.get(req("enterprise", "/ent2")).value should be((cOne, sOneTwo))
    testServiceMatcher.get(req("enterprise", "/check")).value should be((cOne, sOneOne))
    testServiceMatcher.get(req("enterprise", "/loginConfirm")).value should be((cOne, sOneOne))
    testServiceMatcher.get(req("api", "/check")) should be(None)
    testServiceMatcher.get(req("api", "/loginConfirm")) should be(None)
    testServiceMatcher.get(req("api.testdomain", "/apis/test")).value should be((cFour, sFour))
    testServiceMatcher.get(req("api.testdomain", "/signin")).value should be((cFour, sFour))
    testServiceMatcher.get(req("api.testdomain", "/login")) should be(None)
  }

  it should "match the given ServiceIdentifier with itself" in {
    val permutations = for {
      cid <- custIds.toList
      sid <- serviceIds.toList
    } yield getWinner(cid, sid) == Tuple2(cid, sid)
    permutations.foreach(p => p should be(true))
  }
}
