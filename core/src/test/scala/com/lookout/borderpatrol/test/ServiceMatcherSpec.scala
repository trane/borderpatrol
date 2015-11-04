package com.lookout.borderpatrol.test

import com.lookout.borderpatrol.{LoginManager, Manager, ServiceIdentifier, ServiceMatcher}
import com.twitter.finagle.httpx.{RequestBuilder, Request}
import com.twitter.finagle.httpx.path.Path

class ServiceMatcherSpec extends BorderPatrolSuite {

  val keymasterIdManager = Manager("keymaster", Path("/identityProvider"), "localhost:8081")
  val keymasterAccessManager = Manager("keymaster", Path("/accessIssuer"), "localhost:8081")
  val checkpointLoginManager = LoginManager("checkpoint", Path("/check"), "localhost:8081", Path("/loginConfirm"),
    keymasterIdManager, keymasterAccessManager)
  val basicIdManager = Manager("basic", Path("/signin"), "localhost:8081")
  val basicAccessManager = Manager("basic", Path("/accessin"), "localhost:8081")
  val umbrellaLoginManager = LoginManager("umbrella", Path("/umb"), "localhost:8081", Path("/loginIt"),
    keymasterIdManager, keymasterAccessManager)

  val one = ServiceIdentifier("one", "localhost:11", Path("/ent"), "enterprise", checkpointLoginManager)
  val two = ServiceIdentifier("two", "localhost:11", Path("/api"), "api", umbrellaLoginManager)
  val three = ServiceIdentifier("three", "localhost:11", Path("/apis"), "api.subdomain", checkpointLoginManager)
  val four = ServiceIdentifier("four", "localhost:11", Path("/apis/test"), "api.testdomain", umbrellaLoginManager)
  val sids = Set(one, two, three, four)
  val serviceMatcher = ServiceMatcher(sids)

  def req(subdomain: String = "nothing", path: String = "/"): Request =
    RequestBuilder().url(s"http://${subdomain + "."}example.com${path.toString}").buildGet()

  def getWinner(winner: ServiceIdentifier, loser: ServiceIdentifier): ServiceIdentifier =
    serviceMatcher.get(req(winner.subdomain, winner.path.toString)).value

  behavior of "ServiceMatchers"

  it should "match the longest subdomain" in {
    serviceMatcher.subdomain("www.example.com") should be(None)
    serviceMatcher.subdomain("enterprise.api.example.com").value should be(one)
    serviceMatcher.subdomain("enterprise.example.com").value should be(one)
    serviceMatcher.subdomain("api.example.com").value should be(two)
    serviceMatcher.subdomain("api.subdomains.example.com").value should be(two)
    serviceMatcher.subdomain("api.subdomain.example.com").value should be(three)
  }

  it should "match the longest get" in {
    serviceMatcher.get(req("enterprise", "/")) should be(None)
    serviceMatcher.get(req("enterprise", "/ent")).value should be(one)
    serviceMatcher.get(req("enterprise", "/check")).value should be(one)
    serviceMatcher.get(req("enterprise", "/loginConfirm")).value should be(one)
    serviceMatcher.get(req("api", "/check")) should be(None)
    serviceMatcher.get(req("api", "/loginConfirm")) should be(None)
    serviceMatcher.get(req("api.testdomain", "/apis/test")).value should be(four)
    serviceMatcher.get(req("api.testdomain", "/umb")).value should be(four)
    serviceMatcher.get(req("api.testdomain", "/loginIt")).value should be(four)
    serviceMatcher.get(req("api.testdomain", "/login")) should be(None)
  }

  it should "match the given ServiceIdentifier with itself" in {
    val permutations = for {
      winner <- List(one, two, three, four)
      loser <- List(one, two, three, four)
      if winner != loser
    } yield getWinner(winner, loser) == winner
    permutations.foreach(p => p should be(true))
  }

  it should "return None when neither matching" in {
    serviceMatcher.get(req("www", "/applesauce")) should be(None)
  }

  it should "return None when subdomain matches, but path does not" in {
    serviceMatcher.get(req("enterprise", "/apis")) should be(None)
  }
}
