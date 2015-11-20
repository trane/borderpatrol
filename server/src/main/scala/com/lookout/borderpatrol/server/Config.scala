package com.lookout.borderpatrol.server

import java.net.URL

import com.lookout.borderpatrol.{Manager, LoginManager, ServiceIdentifier}
import com.lookout.borderpatrol.sessionx.SecretStores._
import com.lookout.borderpatrol.sessionx.SessionStores._
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

  // Encoder/Decoder for URL
  implicit val encodeUrl: Encoder[URL] = Encoder[String].contramap(_.toString)
  implicit val decodeUrl: Decoder[URL] = Decoder[String].map(new URL(_))

  // Encoder/Decoder for SessionStore
  implicit val encodeSessionStore: Encoder[SessionStore] = Encoder.instance {
    case x: InMemoryStore.type => Json.obj(("type", Json.string("InMemoryStore")))
    case y: MemcachedStore =>  Json.obj(("type", Json.string("MemcachedStore")),
      ("hosts", Json.string("localhost:123")))
    case other => Json.string("Error: " + other.toString)
  }
  implicit val decodeSessionStore: Decoder[SessionStore] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "InMemoryStore" => Xor.right(defaultSessionStore)
      case "MemcachedStore"   => c.downField("hosts").as[String].map(hosts =>
        SessionStores.MemcachedStore(MemcachedClient.newRichClient(hosts)))
      case other  => Xor.left(DecodingFailure(s"Invalid sessionStore: $other", c.history))
    }
  }

  // Encoder/Decoder for SecretStore
  implicit val encodeSecretStore: Encoder[SecretStoreApi] = Encoder.instance {
    case x: InMemorySecretStore => Json.obj(("type", Json.string(x.getClass.getSimpleName)))
    case y: ConsulSecretStore => Json.obj(("type", Json.string(y.getClass.getSimpleName)),
      ("hosts", Json.string(s"${y.consul.host}:${y.consul.port}")))
    case other => Json.string("Error: " + other.toString)
  }
  implicit val decodeSecretStore: Decoder[SecretStoreApi] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "InMemorySecretStore" => Xor.right(defaultSecretStore)
      case "ConsulSecretStore" => c.downField("hosts").as[String].map(hosts =>
        ConsulSecretStore(new URL("http://" + hosts)))
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
        hosts <- c.downField("hosts").as[Set[URL]]
        loginPath <- c.downField("loginPath").as[Path]
        ipName <- c.downField("identityManager").as[String]
        im <- Xor.fromOption(ims.get(ipName),
          DecodingFailure(s"No IdentityManager $ipName found: ", c.history))
        apName <- c.downField("accessManager").as[String]
        am <- Xor.fromOption(ams.get(apName),
          DecodingFailure(s"No AccessManager $apName found: ", c.history)
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
        hosts <- c.downField("hosts").as[Set[URL]]
        path <- c.downField("path").as[Path]
        subdomain <- c.downField("subdomain").as[String]
        lmName <- c.downField("loginManager").as[String]
        lm <- Xor.fromOption(lms.get(lmName),
          DecodingFailure(s"No LoginManager $lmName found: ", c.history)
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
   * Validate Hosts (i.e. Set of URLs) configuration
   * @param field
   * @param name
   * @param hosts
   */
  def validateHostsConfig(field: String, name: String, hosts: Set[URL]): Unit = {
    // Make sure urls in Manager have matching protocol
    if (hosts.map(_.getProtocol()).size != 1)
      throw new InvalidConfigError(s"hosts configuration for ${name} in ${field}: has differing protocols")
    // Make sure hosts in Manager have either http or https protocol
    if (!hosts.map(_.getProtocol()).mkString.matches("http[s]*"))
      throw new InvalidConfigError(s"hosts configuration for ${name} in ${field}: has unsupported protocol")
    // Make sure https hosts have a matching hostname
    if (!hosts.filter(u => u.getProtocol == "https").isEmpty &&
      hosts.map(u => u.getHost()).size != 1)
      throw new InvalidConfigError(
        s"hosts configuration for ${name} in ${field}: https urls have mismatching hostnames")
  }

  /**
   * Validate Manager configuration
   * @param field
   * @param managers
   */
  def validateManagerConfig(field: String, managers: Set[Manager]): Unit = {
    // Find if managers have duplicate entries
    if (managers.size > managers.map(m => m.name).size)
      throw new DuplicateConfigError("name", field)

    // Make sure hosts in Manager have http or https protocol
    managers.map(m => validateHostsConfig(field, m.name, m.hosts))
  }

  /**
   * Validate Login Manager configurartion
   * @param field
   * @param loginManagers
   */
  def validateLoginManagerConfig(field: String, loginManagers: Set[LoginManager]): Unit = {
    // Find if loginManagers have duplicate entries
    if (loginManagers.size > loginManagers.map(lm => lm.name).size)
      throw new DuplicateConfigError("name", field)

    // Make sure hosts in LoginManager have http or https protocol
    loginManagers.map(lm => validateHostsConfig(field, lm.name, lm.hosts))
  }

  /**
   * Validate serviceIdentifier configuration
   * @param field
   * @param sids
   */
  def validateServiceIdentifierConfig(field: String, sids: Set[ServiceIdentifier]): Unit = {
    // Find if ServiceIdentifiers have duplicate entries
    if (sids.size > sids.map(sid => (sid.path, sid.subdomain)).size)
    throw new DuplicateConfigError("path and subdomain", "serviceIdentifiers")

    // Make sure hosts in Serviceidentifier have http or https protocol
    sids.map(sid => validateHostsConfig(field, sid.name, sid.hosts))
  }

  /**
   * Validates the BorderPatrol Configuration
   * - for duplicates
   *
   * @param serverConfig
   */
  def validate(serverConfig: ServerConfig): Unit = {

    //  Validate identityManagers config
    validateManagerConfig("identityManagers", serverConfig.identityManagers)

    //  Validate accessManagers config
    validateManagerConfig("accessManagers", serverConfig.accessManagers)

    //  Validate loginManagers config
    validateLoginManagerConfig("loginManagers", serverConfig.loginManagers)

    //  Validate serviceIdentifiers config
    validateServiceIdentifierConfig("serviceIdentifiers", serverConfig.serviceIdentifiers)
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

