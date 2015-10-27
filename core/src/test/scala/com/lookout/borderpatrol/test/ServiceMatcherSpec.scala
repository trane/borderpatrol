package com.lookout.borderpatrol.test

import com.lookout.borderpatrol.{ServiceIdentifier,ServiceMatcher}
import com.twitter.finagle.httpx.{RequestBuilder, Request}
import com.twitter.finagle.httpx.path.Path

class ServiceMatcherSpec extends BorderPatrolSuite {

  val one = ServiceIdentifier("one", Path("/ent"), "enterprise", Path("/a/login"))
  val two = ServiceIdentifier("two", Path("/api"), "api", Path("/login"))
  val three = ServiceIdentifier("three", Path("/apis"), "api.subdomain", Path("/loginIt"))
  val four = ServiceIdentifier("four", Path("/apis/test"), "api.testdomain", Path("/loginIt/here"))
  val sids = Set(one, two, three, four)
  val serviceMatcher = ServiceMatcher(sids)

  def req(subdomain: String = "nothing", path: String = "/"): Request =
    RequestBuilder().url(s"http://${subdomain + "."}example.com${path.toString}").buildGet()

  def getWinner(winner: ServiceIdentifier, loser: ServiceIdentifier): ServiceIdentifier =
    serviceMatcher.get(req(winner.subdomain, winner.path.toString)).value

  behavior of "ServiceMatchers"

  it should "match the longest path" in {
    serviceMatcher.path(Path("/")) should be(None)
    serviceMatcher.path(Path("/e")) should be(None)
    serviceMatcher.path(Path("/enter")) should be(None)
    serviceMatcher.path(Path("/ent/blah")).value should be(one)
    serviceMatcher.path(Path("/api")).value should be(two)
    serviceMatcher.path(Path("/apis")).value should be(three)
    serviceMatcher.path(Path("/apis/testing")).value should be(three)
    serviceMatcher.path(Path("/apis/test")).value should be(four)
  }

  it should "match the longest login path" in {
    serviceMatcher.login(Path("/")) should be(None)
    serviceMatcher.login(Path("/e")) should be(None)
    serviceMatcher.login(Path("/enter")) should be(None)
    serviceMatcher.login(Path("/a/login/yeah")).value should be(one)
    serviceMatcher.login(Path("/login")).value should be(two)
    serviceMatcher.login(Path("/loginIt")).value should be(three)
    serviceMatcher.login(Path("/loginIt/hereOr")).value should be(three)
    serviceMatcher.login(Path("/loginIt/here")).value should be(four)
  }

  it should "match the longest subdomain" in {
    serviceMatcher.subdomain("www.example.com") should be(None)
    serviceMatcher.subdomain("enterprise.api.example.com").value should be(one)
    serviceMatcher.subdomain("enterprise.example.com").value should be(one)
    serviceMatcher.subdomain("api.example.com").value should be(two)
    serviceMatcher.subdomain("api.subdomains.example.com").value should be(two)
    serviceMatcher.subdomain("api.subdomain.example.com").value should be(three)
  }

  it should "match the given ServiceIdentifier with itself" in {
    val permutations = (for {
      winner <- List(one, two, three, four)
      loser <- List(one, two, three, four)
      if winner != loser
    } yield getWinner(winner, loser) == winner)
    permutations.foreach(p => p should be(true))
  }

  it should "return None when neither matching" in {
    serviceMatcher.get(req("www", "/applesauce")) should be(None)
  }

  it should "match path before subdomain" in {
    serviceMatcher.get(req("enterprise", "/apis")).value should be(three)
    serviceMatcher.get(req("api", "/ent")).value should be(one)
    serviceMatcher.get(req("enterprise", "/api")).value should be(two)
  }

  it should "match subdomain if path doesn't exist" in {
    serviceMatcher.get(req("enterprise", "/path")).value should be(one)
    serviceMatcher.get(req("api", "/rest")).value should be(two)
    serviceMatcher.get(req("api.subdomain", "/taxes")).value should be(three)
  }

}
