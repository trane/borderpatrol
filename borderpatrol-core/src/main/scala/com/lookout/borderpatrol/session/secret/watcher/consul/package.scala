package com.lookout.borderpatrol.session.secret.watcher

import java.util.concurrent.TimeUnit

import argonaut._
import Argonaut._
import com.lookout.borderpatrol.session.Secrets
import com.lookout.borderpatrol.session.secret.SecretsWatcherApi
import com.lookout.borderpatrol.session.secret.SecretsJsonDecode
import com.twitter.bijection.{Base64String, Injection}
import com.twitter.finagle.httpx.{Status, Response, Request}

import scala.concurrent.duration.Duration
import scala.concurrent.{TimeoutException, Await, Promise, Future}
import scala.util.Try

package object consul {
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

  implicit def SecretDataCodecJson: CodecJson[SecretData] =
    casecodec6(SecretData.apply, SecretData.unapply)("CreateIndex", "ModifyIndex", "LockIndex", "Key", "Flags", "Value")

  case class ConsulSecretsWatcher(consulService: ConsulService) extends SecretsWatcherApi {

    val Timeout: Int = 2000
    val BaseUrl: String = "/v1/kv/borderpatrol/secrets"
    val WaitInterval: String = "48h"
    var currentModifyIndex: Int = 0
    var _nextSecrets: Future[Secrets] = Promise[Secrets]().future

    def initialSecrets: Secrets = {
      try {
        val secrets = Await.result[Secrets](getSecrets, Duration(Timeout, TimeUnit.MILLISECONDS))
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
      Try(Await.result[Secrets](next, Duration(Timeout, TimeUnit.MILLISECONDS)))
    }

    private def getSecrets: Future[Secrets] = {
      var promise = Promise[Secrets]()
      //Use a ridiculously long timeout (48 hrs)
      val request = Request(s"$BaseUrl?wait=$WaitInterval&index=$currentModifyIndex")
      consulService(request).onSuccess { resp =>
        handleResponse(resp) match {
          case Left(response) => promise.failure(new IllegalStateException(s"An error occurred getting secrets from the Data Store: ${resp.status}"))
          case Right(content) => {
            for {
              secrets <- getSecretInformation(content)
              secret <- newSecrets(secrets)
            } yield promise.success(secret)
          }
        }
        println(s"Retrieved from Consul: $resp")
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
    private def handleResponse(response: Response): Either[Response, String] = {
      response.status match {
        case Status.Ok => Right(response.contentString)
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
}
