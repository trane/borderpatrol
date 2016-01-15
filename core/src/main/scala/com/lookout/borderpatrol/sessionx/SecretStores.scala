package com.lookout.borderpatrol.sessionx

import java.net.URL
import java.util.concurrent.TimeUnit
import java.util.logging.Logger
import javax.xml.bind.DatatypeConverter

import argonaut.Parse
import io.circe.{Decoder, Json, jawn}
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._

import com.lookout.borderpatrol.BinderBase
import com.lookout.borderpatrol.util.Combinators._
import com.twitter.finagle.httpx.{Method, Status, Request}
import com.twitter.finagle.util.DefaultTimer
import com.twitter.logging.Level
import com.twitter.util._

import scala.util.{Success, Failure}
import scalaz.{\/-, -\/}


/**
 * Two secrets must be in rotation at any given time:
 * - Current: used for creating new sessions and validating incoming non-expired sessions
 * - Previous: validating incoming non-expired sessions, e.g. sessions signed by yesterday's key
 *
 * Since each [[com.lookout.borderpatrol.sessionx.Secret Secret]] expires (default of 1 day), the
 * window of a non-expired [[com.lookout.borderpatrol.sessionx.Session Session]] is somewhere between the expiry of
 * the current `Secret` and the previous `Secret`
 */
trait SecretStoreApi {
  def current: Secret

  def previous: Secret

  def find(f: Secret => Boolean): Option[Secret]
}

/**
 * Default implementations of [[com.lookout.borderpatrol.sessionx.SecretStoreApi SecretStoreApi]]
 */
object SecretStores {
  val ConsulSecretsKey = "secretStore/secrets"

  /**
   * A useful [[com.lookout.borderpatrol.sessionx.Secrets Secrets]] mock store for quickly testing and prototyping
   *
   * @param secrets the current secret and previous secret
   */
  case class InMemorySecretStore(secrets: Secrets) extends SecretStoreApi {
    @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.Var")) // this is for mocking
    private[this] var _secrets: Secrets = secrets

    def current: Secret = {
      val c = _secrets.current
      if (c.expired) {
        _secrets = _secrets.copy(Secret(), c)
        _secrets.current
      }
      else c
    }

    def previous: Secret =
      _secrets.previous

    def find(f: (Secret) => Boolean): Option[Secret] =
      if (f(current)) Some(current)
      else if (f(previous)) Some(previous)
      else None

  }

  /**
   * A store to access the current and previous [[com.lookout.borderpatrol.sessionx.Secrets]] stored in the consul
   * server.
   *
   * @param consulUrl
   * @param _secrets
   */
  case class ConsulSecretStore(consulUrl: URL, var _secrets: Secrets)
      extends SecretStoreApi {
    private[this] val log = Logger.getLogger(getClass.getName)
    private[this] val ConsulSecretsKey = "/v1/kv/BpSecrets"

    /* Alt constructor: this is the commonly used constructor. The main constructor is merely used for unit testing */
    def this(consulUrl: URL) = this(consulUrl, Secrets(Secret(Time.now), Secret(Time.now)))

    /* Kick off a poll timer */
    DefaultTimer.twitter.schedule(Time.now)(pollSecrets)

    /**
     * Get the current secret from the cache layer
     */
    def current: Secret =
      _secrets.current

    /**
     * Get the previous secret from the cache layer
     */
    def previous: Secret =
      _secrets.previous

    /**
     * Look for the Secret being checked in the function.
     */
    def find(f: (Secret) => Boolean): Option[Secret] =
      if (f(current)) Some(current)
      else if (f(previous)) Some(previous)
      else None


    /**
     * Poll worker that keep the in sync with the "secrets" in Consul. It uses 3 step process to do so:
     * 1. get latest secrets from consul (if valid, set the timer until expiry)
     * 2. update/put the new secrets in consul (if success, then that becomes new secret)
     * 3. get the secrets again (if step2 failed, set the timer until secrets expiry)
     *
     * On any failure, the whole process is repeated after 1 minute
     */
    def pollSecrets(): Unit =
      (for {
        (consulResponse, secrets, result) <- getSecretsFromConsul(1, false, None)
        (secrets, result) <- putSecretsOnConsul(2, result, consulResponse, secrets)
        (_, secrets, result) <- getSecretsFromConsul(3, result, secrets)
      } yield (secrets, result) match {
          case (Some(s), true) =>
            log.log(Level.DEBUG,
              s"ConsulSecretStore: Received a new Secret from Consul with an expiry: ${s.current.expiry}")
            _secrets = s
            DefaultTimer.twitter.schedule(_secrets.current.expiry)(pollSecrets)

          case _ =>
            log.log(Level.DEBUG, "ConsulSecretStore: Failed to get Secrets from Consul, trying again soon in 1 minute")
            DefaultTimer.twitter.schedule(Time.now + Duration(1, TimeUnit.MINUTES))(pollSecrets)

        }) handle {
        case e: Throwable =>
          log.log(Level.ERROR, s"ConsulSecretStore: Failed to get Secrets from Consul with: ${e.getMessage}, " +
            s"trying again soon in 1 minute")
          DefaultTimer.twitter.schedule(Time.now + Duration(1, TimeUnit.MINUTES))(pollSecrets)
      }

    /**
     * GET the Secrets (i.e. value) for a pre-defined ConsulSecretsKey from a KV store on Consul,
     * parse it and validate the contents
     *
     * @param step
     * @param result
     * @param inputSecrets
     * @return
     */
    private[this] def getSecretsFromConsul(step: Int, result: Boolean, inputSecrets: Option[Secrets]):
        Future[(Option[ConsulResponse], Option[Secrets], Boolean)] =
      /* Skip this step, if result is already successful */
      if (result) {
        (None, inputSecrets, true).toFuture
      } else {

        /* Get the secrets from Consul */
        for {
          consulResponseList <- BinderBase.connect(consulUrl.toString, Set(consulUrl),
                                                   Request(ConsulSecretsKey)).flatMap(
            res => res.status match {
              case Status.Ok =>
                jawn.decode[List[ConsulResponse]](res.contentString).fold[Future[List[ConsulResponse]]](
                  err => Future.exception(ConsulError(//Status.InternalServerError,
                    s"Step-${step}: Failed to parse the Consul response: ${err.getMessage}")),
                  t => Future.value(t))

              case Status.NotFound => List.empty.toFuture

              case _ => Future.exception(ConsulError(
                s"Step-${step}: Failed with an error response from Consul: ${res.status}"))

            })
          consulResponse <- consulResponseList.headOption.toFuture
          secrets <- consulResponse match {
            case None => None.toFuture
            case Some(cr) => Some(cr.secretsForValue()).toFuture
          }
          result <- secrets.fold(false)(!_.current.expired).toFuture
        } yield (consulResponse, secrets, result)
      }

    /**
     * PUT or try to set the Secrets (i.e. value) in the KV store on Consul. If it succeeds, then this node will
     * act as leader and take charge of setting the Secrets. If it fails, then it probably lost the race with other
     * node.
     *
     * It uses `cas` query parameter (i.e. modifyIndex) to make it an atomic opertion.
     *
     * @param step step number, mainly for debugging
     * @param result if earlier operations succeeded are not (do we already have a secret)
     * @param inputConsulResponse ConsulResponse fetched by earlier operations
     * @param inputSecrets Input
     * @return
     */
    private[this] def putSecretsOnConsul(step: Int, result: Boolean, inputConsulResponse: Option[ConsulResponse],
                                         inputSecrets: Option[Secrets]):
        Future[(Option[Secrets], Boolean)] =
      /* Skip this step, if result is already successful */
      if (result) {
        (inputSecrets, true).toFuture
      } else {

        /* Generate next secrets */
        val newSecrets = inputSecrets match {
          case None => Secrets(Secret(), _secrets.current)
          case Some(s) => Secrets(Secret(), s.current)
        }

        /* Find out new modifyIndex */
        val modifyIndex = inputConsulResponse match {
          case None => 0
          case Some(oc) => oc.modifyIndex
        }

        /* Try setting the next secrets */
        BinderBase.connect(consulUrl.toString, Set(consulUrl),
          tap(Request(Method.Put, Request.queryString(ConsulSecretsKey, ("cas" -> modifyIndex.toString))))(req => {
            req.contentString = SecretsEncoder.EncodeJson.encode(newSecrets).toString
            req.contentType = "text/plain"
          })).flatMap(
            res => res.status match {
              case Status.Ok =>
                log.log(Level.DEBUG, "ConsulSecretStore: LEADER: Updated the new Secrets on Consul with an expiry: " +
                  newSecrets.current.expiry)
                Future.value((Some(newSecrets), res.contentString == "true"))

              case _ =>
                Future.exception(ConsulError(
                  s"Step-${step}: Failed with an error response from Consul: ${res.status}"))
            }
          )
      }
  }
}

/**
 * Consul Response
 */
case class ConsulResponse(createIndex: Int, flags: Int, key: String, lockIndex: Int, modifyIndex: Int, value: String) {

  def secretsForValue(): Secrets = {
    val jsonString = new String(DatatypeConverter.parseBase64Binary(value)).map(_.toChar)
    Parse.parse(jsonString) match {
      case -\/(e) => throw ConsulError(
        s"Expected JSON string, but received invalid string Value from Consul with: ${e}")
      case \/-(j) => SecretsEncoder.EncodeJson.decode(j) match {
        case Failure(e) => throw ConsulError(s"Failed to decode ConsulResponse from the JSON string with: $e")
        case Success(v) => v
      }
    }
  }
}

object ConsulResponse {

  /* Alt constructor */
  def apply(createIndex: Int, flags: Int, key: String, lockIndex: Int, modifyIndex: Int,
            secrets: Secrets): ConsulResponse =
    ConsulResponse(createIndex, flags, key, lockIndex, modifyIndex,
      DatatypeConverter.printBase64Binary(SecretsEncoder.EncodeJson.encode(secrets).nospaces.getBytes))

  /**
   * Consul Response Encoder/Decoder
   */
  implicit val ConsulResponseEncoder: io.circe.Encoder[ConsulResponse] =
    io.circe.Encoder.instance { t =>
      Json.fromFields(Seq(
        ("CreateIndex", t.createIndex.asJson),
        ("Flags", t.flags.asJson),
        ("Key", t.key.asJson),
        ("LockIndex", t.lockIndex.asJson),
        ("ModifyIndex", t.modifyIndex.asJson),
        ("Value", t.value.asJson)))
    }
  implicit val ConsulResponseDecoder: Decoder[ConsulResponse] = Decoder.instance { c =>
    for {
      createIndex <- c.downField("CreateIndex").as[Int]
      flags <- c.downField("Flags").as[Int]
      key <- c.downField("Key").as[String]
      lockIndex <- c.downField("LockIndex").as[Int]
      modifyIndex <- c.downField("ModifyIndex").as[Int]
      value <- c.downField("Value").as[String]
    } yield ConsulResponse(createIndex, flags, key, lockIndex, modifyIndex, value)
  }
}
