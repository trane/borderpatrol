package com.lookout.borderpatrol.session

import com.twitter.finagle.Service
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Created by wkimeria on 1/15/15.
 */
class SecretsWatcherApiSpec extends FlatSpec with Matchers{

  behavior of "SecretWatcherApi"

  def MockConsulService(response: HttpResponse) = new Service[HttpRequest, HttpResponse] {
    def apply(request: HttpRequest) = {
      val content =
        """{'current':{'expiry':{'nanos':100000000000},
          #'id':-29,'entropy':[67,93,65,26,89,123,-88,-59,-82,103,-81,-43,-113,-98,-21,-19]},
          #'previous':{'expiry':{'nanos':100000000000},'id':-96,
          #'entropy':[-66,5,60,86,12,62,-85,67,72,57,-19,-5,-47,-26,-101,63]}}""".stripMargin('#')

      Future.value(response)
    }
  }

  it should "get secrets" in {
    //TODO
  }

}
