package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit
import com.lookout.borderpatrol.session.secret._
import com.lookout.borderpatrol.session.secret.watcher.consul.{SecretDataCodecJson, SecretData, ConsulSecretsWatcher, ConsulService}
import com.lookout.borderpatrol.session.secret.SecretsWatcherApi
import com.twitter.bijection.{Base64String, Injection}
import com.twitter.finagle.httpx.netty.Bijections
import com.twitter.finagle.httpx.{Request, Status, Response}
import com.twitter.util.{Duration, Future, Time}
import org.jboss.netty.handler.codec.http._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

class SecretsWatcherApiSpec extends FlatSpec with Matchers {

  behavior of "SecretWatcherApi"

  val coder = Injection.connect[String, Array[Byte], Base64String, String]

  def secrets: Secrets = {
    def currentExpiry: Time = Time.now + Duration(1, TimeUnit.DAYS)
    def mockSecret = Secret(currentExpiry)
    Secrets(mockSecret, mockSecret)
  }

  case class MockConsulService(f: Response => Response) extends ConsulService {
    override def apply(request: Request) = {
      Future.value(f(Response(Status.Ok)))
    }
  }

  def respondWithGoodSecrets(secrets: Secrets)(response: Response): Response = {
    val sData = coder(secrets.asJson)
    val json = "[" + SecretDataCodecJson.encode(SecretData(1, 1, 1, "secrets", 1, sData)) + "]"
    response.contentString = json.toString
    response
  }

  def respondWithBadSecrets(response: Response): Response = {
    val sData = coder("{'not_secrets': 'bad'}")
    val json = "[" + SecretDataCodecJson.encode(SecretData(1, 1, 1, "secrets", 1, sData)) + "]"
    response.contentString = json.toString
    response
  }

  it should "get valid initial secrets" in {
    val goodSecrets = secrets
    val watcher: SecretsWatcherApi = ConsulSecretsWatcher(MockConsulService(respondWithGoodSecrets(goodSecrets)))
    val initialSecrets = watcher.initialSecrets
    initialSecrets shouldEqual (goodSecrets)
  }

  it should "error on unparseable initial secrets" in {
    val goodSecrets = secrets
    val watcher: SecretsWatcherApi = ConsulSecretsWatcher(MockConsulService(respondWithBadSecrets))
    intercept[Exception] {
      val initialSecrets = watcher.initialSecrets
    }
  }

  it should "get None on getNext if no new secrets" in {
    val goodSecrets = secrets
    val watcher: SecretsWatcherApi = ConsulSecretsWatcher(MockConsulService(respondWithGoodSecrets(goodSecrets)))
    watcher.initialSecrets
    val next = watcher.getNext match {
      case Success(newSecrets) => newSecrets
      case Failure(f) => {
        None
      }
    }
    next shouldEqual (None)
  }
}