package com.lookout.borderpatrol.sessionx

import argonaut.Argonaut._
import argonaut.{CodecJson, Parse}
import com.twitter.finagle.{httpx, Service}
import com.twitter.io.Buf
import com.twitter.util.Future
import scala.util.{Try, Failure, Success}
import scalaz._


/**
 * Stores the secrets stored on the consul server.
 * Polls the consul server and keeps an inmemory cache of Secrets
 *
 * @param poll How often in seconds to check for updates
 * @param consul An instance on ConsulConnection to make connections with the consul server
 */

case class ConsulSecretCache(poll: Long, consul: ConsulConnection, secretsStart: Secrets) extends Runnable {

  val cacheBuffer = collection.mutable.ArrayBuffer[Secrets](secretsStart)
  val newBuffer = collection.mutable.ArrayBuffer[Secrets]()

  /**
   * Checks if the current secret is expired and if there is a new secret available and rotates the secrets.
   * Then returns the latest secret
   */
  def secrets: Secrets = {
    val s = cacheBuffer.last
    if (s.current.expired) rotateSecret
    else s
  }

  /**
   * Checks if the secret exists. Rotates if the secret is the new secret.
   */
  def find(f: Secret => Boolean): Option[Secret] = {
    val lastSecrets = cacheBuffer.last

    if (f(lastSecrets.current)) Some(lastSecrets.current)
    else if (f(lastSecrets.previous)) Some(lastSecrets.previous)
    else if (f(rotateSecret.current)) Some(cacheBuffer.last.current)
    else None
  }

  /**
   * Continuously poll the consul server at the interval passed to the Class when it was created
   * updates the store for a possibly new Secret to be used
   */
  def run(): Unit =
    while (true) {
      for {
        n <- pollSecrets
      } yield n match {
        case Some(s) => newBuffer.append(s)
        case None => ()
      }
      Thread.sleep(poll)
    }

  private def needsRotation: Boolean =
    newBuffer.nonEmpty && newBuffer.last.current != cacheBuffer.last.current

  private def rotateSecret: Secrets = {
    if (needsRotation) {
      this.synchronized {
        if (needsRotation)
          cacheBuffer.append(newBuffer.last)
      }
    }
    cacheBuffer.last
  }

  /**
   * Get the secret at current from the consul server or returns None
   */
  private def pollSecrets: Future[Option[Secrets]] =
    consul.value(SecretStores.ConsulSecretsKey).map({
      case Success(a) => secretsTryFromString(a).toOption
      case Failure(e) => None
    })

  /**
   * Returns a Try[Secret] from json as a [String]
   *
   * @param s A json string with information to create a Secret
   */
  private def secretsTryFromString(s: String): Try[Secrets] = {
    Parse.parse(s) match {
      case -\/(e) => util.Failure(new Exception(s"Failed with: $e"))
      case \/-(j) => SecretsEncoder.EncodeJson.decode(j)
    }
  }
}

/**
 * class to interface with consul kv store
 *
 * @param consul Finagle server to send and get requests to the server
 * @param host host name of the consul server to connect to ex: "localhost"
 */
case class ConsulConnection(consul: Service[httpx.Request, httpx.Response], host: String, port: String) {

  /**
   * Return a json String from a consul key URL
   *
   * @param k key to get a consul json response from
   */
  private def consulResponse(k: String): Future[Buf] = {
    val req = httpx.Request(httpx.Method.Get, k)
    req.host = host
    consul(req).map(a => a.content)
  }

  private def base64ToString(s: String): Option[String] = {
    val buf = com.twitter.util.Base64StringEncoder.decode(s)
    Buf.Utf8.unapply(Buf.ByteArray.Owned(buf))
  }

  /**
   * Get just the decoded value for a key from consul as Future[String]. To get the full json response from Consul
   * use getConsulRepsonse
   *
   * @param key the key to get the value for
   */
  def value(key: String): Future[Try[String]] =
    consulResponse("/v1/kv/" + key) map { buf =>
      (for {
        body <- Buf.Utf8.unapply(buf)
        rlist <- body.decodeOption[List[ConsulResponse]]
        res <- rlist.headOption
        value <- base64ToString(res.value)
      } yield value) match {
        case None => Failure(ConsulError("Unable to decode consul response"))
        case Some(f) => Success(f)
      }
    }

  /**
   * Set the given key to the given Value. Both are strings
   *
   * @param k the key to set
   * @param v the value of the key
   */
  def set(k: String, v: String): Future[httpx.Response] = {
    val currentData = Buf.Utf8(v)
    val update: httpx.Request = httpx.RequestBuilder()
      .url(s"http://$host:$port/v1/kv/$k")
      .buildPut(currentData)
    consul(update)
  }
}

/**
 * Class to use for Argonaut json conversion
 */
case class ConsulResponse(
    flags: Int, modifyIndex: Int, value: String, lockIndex: Int, createIndex: Int, key: String)

object ConsulResponse {
  implicit val ConsulResponseCodec: CodecJson[ConsulResponse] =
    casecodec6(ConsulResponse.apply, ConsulResponse.unapply)(
      "Flags", "ModifyIndex", "Value", "LockIndex", "CreateIndex", "Key")
}
