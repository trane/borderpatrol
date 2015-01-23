package com.lookout.borderpatrol.session.secret

import java.util.concurrent.TimeoutException

import argonaut._
import com.lookout.borderpatrol.ConsulService
import com.lookout.borderpatrol.session.Secrets
import com.twitter.bijection.{Injection, Base64String}
import com.twitter.io.Charsets
import org.jboss.netty.handler.codec.http._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future => ScalaFuture, Promise => ScalaPromise}
import scala.util.Try

sealed trait SecretsWatcherApi {
  def initialSecrets: Secrets

  def getNext: Try[Secrets]
}

case class ConsulSecretsWatcher(consulService: ConsulService) extends SecretsWatcherApi {

  val Timeout: Int = 2000
  val BaseUrl: String = "/v1/kv/borderpatrol/secrets"
  val WaitInterval: String = "48h"
  var currentModifyIndex: Int = 0
  var _nextSecrets: ScalaFuture[Secrets] = ScalaPromise[Secrets]().future

  def initialSecrets: Secrets = {
    try {
      val secrets = Await.result[Secrets](getSecrets, Duration(Timeout, MILLISECONDS))
      _nextSecrets = getSecrets
      secrets
    } catch {
      case e: TimeoutException => throw new Exception("Timeout occurred trying to retrieve secrets: " + e)
      case e: Exception => throw new Exception("Unable to retrieve Secrets: " + e)
    }
  }

  def getNext: Try[Secrets] = {
    val next = _nextSecrets
    _nextSecrets = getSecrets
    Try(Await.result[Secrets](next, Duration(Timeout, MILLISECONDS)))
  }

  private def getSecrets: ScalaFuture[Secrets] = {
    var promise = ScalaPromise[Secrets]()
    //Use a ridiculously long timeout (48 hrs)
    val request = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, s"$BaseUrl?wait=$WaitInterval&index=$currentModifyIndex")
    consulService(request).onSuccess { resp =>
      handleResponse(resp) match {
        case Left(response) => promise.failure(new IllegalStateException("An error occurred getting secrets from the Data Store:" + resp.getStatus))
        case Right(content) => {
          for {
            secrets <- getSecretInformation(content)
            secret <- newSecrets(secrets)
          } yield promise.success(secret)
        }
      }
      println("Retrieved from Consul: " + resp.getContent.toString(Charsets.Utf8))
    }
    promise.future
  }

  private def newSecrets(secrets: SecretData): Option[Secrets] = {
    println(s"currentModify=$currentModifyIndex and modifyIndex = ${secrets.ModifyIndex}")
    if (currentModifyIndex == 0 || secrets.ModifyIndex > currentModifyIndex) {
      currentModifyIndex = secrets.ModifyIndex
      val coder = Injection.connect[String, Array[Byte], Base64String, String]
      coder.invert(secrets.Value).get.asSecrets
    }
    else
      None
  }

  /**
   * Handle http response. If 200, return body content, otherwise, return response
   * @param response
   * @return
   */
  private def handleResponse(response: HttpResponse): Either[HttpResponse, String] = {
    response.getStatus match {
      case HttpResponseStatus.OK => Right(response.getContent.toString(Charsets.Utf8))
      case _ => Left(response)
    }
  }

  /**
   * Parse Response from Consul and extract new secret and
   * current ModifyIndex
   * @param s
   * @return
   */
  def getSecretInformation(s: String): Option[SecretData] = {
    for {
      res <- Parse.decodeOption[List[SecretData]](s)
      sData <- res.headOption
    } yield sData
  }
}

/**
 * The SecretData class represents a row of data coming back from the Secret Store back-end (Consul)
 * The JSON Data structure looks like below (value is Base64 Encoded)
 * [{"CreateIndex":66,"ModifyIndex":68,"LockIndex":0,"Key":"borderpatrol/currentSecret","Flags":0,"Value":"YmJiYmJi......"}]
 * THE Value is Base64 encoded, when decoded, it looks like
 * {"current":{"expiry":{"nanos":100000000000},"id":-29,"entropy":[67,93,65,26,89,123,-88,-59,-82,103,-81,-43,-113,-98,-21,-19]},
 * "previous":{"expiry":{"nanos":100000000000},"id":-96,"entropy":[-66,5,60,86,12,62,-85,67,72,57,-19,-5,-47,-26,-101,63]}}
 */
case class SecretData(val CreateIndex: Int,
                      val ModifyIndex: Int,
                      val LockIndex: Int,
                      val Key: String,
                      val Flags: Int,
                      val Value: String)