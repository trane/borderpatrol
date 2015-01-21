package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit

import com.lookout.borderpatrol.Response
import com.twitter.finagle.Service
import com.twitter.io.Charsets
import com.twitter.util.Future
import com.twitter.util.{Duration, Time}
import org.jboss.netty.buffer.ChannelBuffers._
import org.jboss.netty.handler.codec.http._
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by wkimeria on 1/15/15.
 */
class SecretsWatcherApiSpec extends FlatSpec with Matchers{

  behavior of "SecretWatcherApi"

  def MockConsulService(response: HttpResponse) = new Service[HttpRequest, HttpResponse] {
    def apply(request: HttpRequest) = {
      Future.value(respondWithGoodSecrets(new Response(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK))))
    }

    private def respondWithGoodSecrets(response: Response): HttpResponse = {
      def currentExpiry: Time = Time.now + Duration(1, TimeUnit.DAYS)
      def mockSecret = Secret(currentExpiry)
      val secret = Secrets(mockSecret, mockSecret)
      response.setContent(copiedBuffer(secret.asJson, Charsets.Utf8))
      response
    }
  }

  it should "get secrets" in {
    //TODO
  }

}
