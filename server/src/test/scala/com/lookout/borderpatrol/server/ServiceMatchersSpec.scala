package com.lookout.borderpatrol.server

import com.lookout.borderpatrol.server.models.ServiceIdentifier
import com.lookout.borderpatrol.test.BorderPatrolSuite
import com.twitter.finagle.httpx.{RequestBuilder, Request}
import com.twitter.finagle.httpx.path.Path
import com.twitter.util.Await
import io.finch.response.Ok
import io.finch.route._
import ServiceMatchers._

class ServiceMatchersSpec extends BorderPatrolSuite {

  val one = ServiceIdentifier("one", Path("/ent"), "enterprise")
  val two = ServiceIdentifier("two", Path("/api"), "api")
  val three = ServiceIdentifier("three", Path("/apis"), "api.subdomain")
  implicit val sids = Set(one, two, three )

  object servicePath extends Extractor("service", ServicesMatcher.path)
  object rest extends TailExtractor("restOfPath", identity)
  val subdomain = DefaultService("defaultService", ServicesMatcher.subdomain)

  val router = (servicePath | subdomain) / rest /> { (sub, path) =>
    Ok(sub.getOrElse(
      if (path.isEmpty) "default"
      else path.toString)
    )
  }
  val service = router.toService

  def req(subdomain: String = "nothing", path: Path = Path("/")): Request =
    RequestBuilder().url(s"http://${subdomain + "."}example.com${path.toString}").buildGet()

  behavior of "ServiceMatchers"

  it should "default to subdomain match when path is /" in {
    val fResults = for {
      a <- service(req(one.subdomain))
      b <- service(req(two.subdomain))
      c <- service(req(three.subdomain))
      d <- service(req(one.subdomain + "s"))
      e <- service(req()) // failed
    } yield List(a, b, c, d, e)
    val results = Await.result( fResults.map(_.map(_.contentString)) )
    results should be(List(one.name, two.name, three.name, "default", "default"))
  }

  it should "return the service name matching the exact path" in {
    val fResults = for {
      a <- service(req("a", one.path))
      b <- service(req("a", two.path))
      c <- service(req("a", three.path))
      d <- service(req("a", Path(one.path.toString + "/other")))
      e <- service(req("a", Path("/"))) // failed
    } yield List(a, b, c, d, e)
    val results = Await.result( fResults.map(_.map(_.contentString)) )
    results should be(List(one.name, two.name, three.name, one.name, "default"))
  }

  it should "pass along the rest of the path when service path matching succeeds" in {
    // /service/rest/of/path => match "service", => /rest/of/path
    val service2 = ((servicePath | subdomain) / rest /> { (sub, path) => Ok(path.toString) }).toService
    val pathTail = "/rest/of/path"
    val inputPath = Path(one.path.toString + pathTail)

    val expected = Path(pathTail).toList.toString
    val results = Await.result(
      service2(req("a", inputPath)).map(_.contentString)
    )
    results should be(expected)
  }

}
