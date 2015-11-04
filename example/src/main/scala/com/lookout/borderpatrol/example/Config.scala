package com.lookout.borderpatrol.example

import com.lookout.borderpatrol.{Manager, LoginManager, ServiceIdentifier}
import com.lookout.borderpatrol.sessionx.SecretStores.InMemorySecretStore
import com.lookout.borderpatrol.sessionx.SessionStores.MemcachedStore
import com.lookout.borderpatrol.sessionx.SessionStores.InMemoryStore
import com.lookout.borderpatrol.sessionx._
import com.twitter.finagle.MemcachedClient
import com.twitter.finagle.httpx.path.Path
import com.twitter.app.App
import cats.data.Xor
import io.circe.{Encoder, _}
import io.circe.jawn._
import io.circe.generic.auto._
import io.circe.syntax._
import scala.io.Source


// scalastyle:off null
case class ConfigError(message: String)
  extends Exception(s"An error occurred while reading BorderPatrol Configuration: ${message}", null)

case class DuplicateConfigError(key: String, field: String)
  extends Exception("An error occurred while reading BorderPatrol Configuration: " +
    s"Duplicate entries for key(s) (${key}) - are found in the field: ${field}")

case class InvalidConfigError(message: String)
  extends Exception(message, null)

case class ServerConfig(secretStore: SecretStoreApi,
                   sessionStore: SessionStore,
                   serviceIdentifiers: Set[ServiceIdentifier],
                   loginManagers: Set[LoginManager],
                   identityManagers: Set[Manager],
                   accessManagers: Set[Manager]) {

  def findIdentityManager(n: String): Manager = identityManagers.find(_.name == n)
    .getOrElse(throw new InvalidConfigError("Failed to find IdentityManager for: " + n))

  def findAccessManager(n: String): Manager = accessManagers.find(_.name == n)
    .getOrElse(throw new InvalidConfigError("Failed to find Manager for: " + n))

  def findLoginManager(n: String): LoginManager = loginManagers.find(_.name == n)
    .getOrElse(throw new InvalidConfigError("Failed to find LoginManager for: " + n))
}

/**
 * Where you will find the Secret Store and Session Store
 */
object Config {

  val defaultConfigFile = "bpConfig.json"
  val defaultSecretStore = SecretStores.InMemorySecretStore(Secrets(Secret(), Secret()))
  val defaultSessionStore = SessionStores.InMemoryStore
  val defaultServiceIdsFile = "bpConfig.json"
  val serverConfigFields = Set("secretStore", "sessionStore", "serviceIdentifiers", "loginManagers",
    "identityManagers", "accessManager")

  // Encoder/Decoder for Path
  implicit val encodePath: Encoder[Path] = Encoder[String].contramap(_.toString)
  implicit val decodePath: Decoder[Path] = Decoder[String].map(Path(_))

  // Encoder/Decoder for SessionStore
  implicit val encodeSessionStore: Encoder[SessionStore] = Encoder.instance {
    case x: InMemoryStore.type => Json.obj(("type", Json.string(x.getClass.getSimpleName)))
    case y: MemcachedStore =>  Json.obj(("type", Json.string(y.getClass.getSimpleName)),
      ("hosts", Json.string("localhost:123")))
    case other => Json.string("Error: " + other.toString)
  }
  implicit val decodeSessionStore: Decoder[SessionStore] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "InMemoryStore$" => Xor.right(defaultSessionStore)
      case "MemcachedStore"   => c.downField("hosts").as[String].map(hosts =>
        SessionStores.MemcachedStore(MemcachedClient.newRichClient(hosts)))
      case other  => Xor.left(DecodingFailure(s"Invalid sessionStore: $other", c.history))
    }
  }

  // Encoder/Decoder for SecretStore
  implicit val encodeSecretStore: Encoder[SecretStoreApi] = Encoder.instance {
    case x: InMemorySecretStore => Json.obj(("type", Json.string(x.getClass.getSimpleName)))
    case other => Json.string("Error: " + other.toString)
  }
  implicit val decodeSecretStore: Decoder[SecretStoreApi] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "InMemorySecretStore" => Xor.right(defaultSecretStore)
      case other  => Xor.left(DecodingFailure(s"Invalid secretStore: $other", c.history))
    }
  }

  /**
   * Encoder/Decoder for LoginManager
   *
   * Note that Decoder for LoginManager does not work standalone, it can be only used
   * while decoding the entire ServerConfig due to dependency issues
   */
  implicit val encodeLoginManager: Encoder[LoginManager] = Encoder.instance { lm =>
    Json.fromFields(Seq(
      ("name", lm.name.asJson),
      ("path", lm.path.asJson),
      ("hosts", lm.hosts.asJson),
      ("loginPath", lm.loginPath.asJson),
      ("identityManager", lm.identityManager.name.asJson),
      ("accessManager", lm.accessManager.name.asJson)))
  }
  def decodeLoginManager(ims: Map[String, Manager], ams: Map[String, Manager]): Decoder[LoginManager] =
    Decoder.instance { c =>
      for {
        name <- c.downField("name").as[String]
        path <- c.downField("path").as[Path]
        hosts <- c.downField("hosts").as[String]
        loginPath <- c.downField("loginPath").as[Path]
        ipName <- c.downField("identityManager").as[String]
        im <- Xor.fromOption(
          ims.get(ipName),
          DecodingFailure(s"No IdentityManager $ipName found", c.history)
        )
        apName <- c.downField("accessManager").as[String]
        am <- Xor.fromOption(
          ams.get(apName),
          DecodingFailure(s"No AccessManager $apName found", c.history)
        )
      } yield LoginManager(name, path, hosts, loginPath, im, am)
    }

  // Encoder/Decoder for ServiceIdentifier
  implicit val encodeServiceIdentifier: Encoder[ServiceIdentifier] = Encoder.instance { sid =>
    Json.fromFields(Seq(
      ("name", sid.name.asJson),
      ("hosts", sid.hosts.asJson),
      ("path", sid.path.asJson),
      ("subdomain", sid.subdomain.asJson),
      ("loginManager", sid.loginManager.name.asJson)))
  }
  def decodeServiceIdentifier(lms: Map[String, LoginManager]): Decoder[ServiceIdentifier] =
    Decoder.instance { c =>
      for {
        name <- c.downField("name").as[String]
        hosts <- c.downField("hosts").as[String]
        path <- c.downField("path").as[Path]
        subdomain <- c.downField("subdomain").as[String]
        lmName <- c.downField("loginManager").as[String]
        lm <- Xor.fromOption(
          lms.get(lmName),
          DecodingFailure(s"No LoginManager $lmName found", c.history)
        )
      } yield ServiceIdentifier(name, hosts, path, subdomain, lm)
    }

  /**
   * Decoder for ServerConfig (Using circe default encoder for encoding)
   */
  implicit val serverConfigDecoder: Decoder[ServerConfig] = Decoder.instance { c =>
    for {
      secretStore <- c.downField("secretStore").as[SecretStoreApi]
      sessionStore <- c.downField("sessionStore").as[SessionStore]
      ims <- c.downField("identityManagers").as[Set[Manager]]
      ams <- c.downField("accessManagers").as[Set[Manager]]
      lms <- c.downField("loginManagers").as(Decoder.decodeSet(
        decodeLoginManager(ims.map(im => im.name -> im).toMap, ams.map(am => am.name -> am).toMap)))
      sids <- c.downField("serviceIdentifiers").as(Decoder.decodeSet(
        decodeServiceIdentifier(lms.map(lm => lm.name -> lm).toMap)))
    } yield ServerConfig(secretStore, sessionStore, sids, lms, ims, ams)
  }

  /**
   * Validates the BorderPatrol Configuration
   * - for duplicates
   *
   * @param serverConfig
   */
  def validate(serverConfig: ServerConfig): Unit = {
    // Find if IdentityManagers have duplicate entries
    if (serverConfig.identityManagers.size > serverConfig.identityManagers.map(im => im.name).size)
      throw new DuplicateConfigError("name", "identityManagers")

    // Find if Managers have duplicate entries
    if (serverConfig.accessManagers.size > serverConfig.accessManagers.map(am => am.name).size)
      throw new DuplicateConfigError("name", "accessManagers")

    // Find if LoginManagers have duplicate entries
    if (serverConfig.loginManagers.size > serverConfig.loginManagers.map(lm => lm.name).size)
      throw new DuplicateConfigError("name", "loginManagers")

    // Find if ServiceIdentifiers have duplicate entries
    if (serverConfig.serviceIdentifiers.size >
      serverConfig.serviceIdentifiers.map(sid => (sid.path, sid.subdomain)).size)
      throw new DuplicateConfigError("path and subdomain", "serviceIdentifiers")
  }

  /**
   * Reads BorderPatrol configuration from the given filename
   *
   * @param filename
   * @return ServerConfig
   */
  def readServerConfig(filename: String) : ServerConfig = {
    decode[ServerConfig](Source.fromFile(filename).mkString) match {
      case Xor.Right(a) => validate(a); a
      case Xor.Left(b) => throw ConfigError("Failed to decode following fields: " +
        (serverConfigFields.filter(b.getMessage contains _).reduceOption((a, b) => s"$a, $b") getOrElse "unknown"))
    }
  }
}

/**
 * A [[com.twitter.app.App]] mixin to use for Configuration. Defines flags
 * to configure the BorderPatrol Server
 */
trait Config {self: App =>
  import Config._

  // Flag for Secret Store
  val configFile = flag("configFile", defaultConfigFile,
    "BorderPatrol config file in JSON format")
}

