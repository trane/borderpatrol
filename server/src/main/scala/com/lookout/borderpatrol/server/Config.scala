package com.lookout.borderpatrol.server

import java.net.URL

import com.lookout.borderpatrol._
import com.lookout.borderpatrol.sessionx.SecretStores._
import com.lookout.borderpatrol.sessionx.SessionStores._
import com.lookout.borderpatrol.sessionx._
import com.twitter.finagle.Memcached
import com.twitter.finagle.Name
import com.twitter.finagle.Resolver
import com.twitter.finagle.http.path.Path
import com.twitter.app.App
import cats.data.Xor
import io.circe.{Encoder, _}
import io.circe.jawn._
import io.circe.generic.auto._
import io.circe.syntax._
import scala.io.Source


case class ServerConfig(secretStore: SecretStoreApi,
                        sessionStore: SessionStore,
                        statsdExporterConfig: StatsdExporterConfig,
                        customerIdentifiers: Set[CustomerIdentifier],
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

case class StatsdExporterConfig(host: String, durationInSec: Int, prefix: String)

/**
 * Where you will find the Secret Store and Session Store
 */
object Config {

  val defaultConfigFile = "bpConfig.json"
  val defaultSecretStore = SecretStores.InMemorySecretStore(Secrets(Secret(), Secret()))
  val defaultSessionStore = SessionStores.InMemoryStore
  val serverConfigFields = Set("secretStore", "sessionStore", "customerIdentifiers", "serviceIdentifiers",
    "loginManagers", "identityManagers", "accessManager", "statsdReporter")

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
  }
  implicit val decodeSessionStore: Decoder[SessionStore] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "InMemoryStore" => Xor.right(defaultSessionStore)
      case "MemcachedStore"   => c.downField("hosts").as[String].map(hosts =>
        SessionStores.MemcachedStore(Memcached.client.newRichClient(hosts)))
      case other  => Xor.left(DecodingFailure(s"Invalid sessionStore: $other", c.history))
    }
  }

  // Encoder/Decoder for SecretStore
  implicit val encodeSecretStore: Encoder[SecretStoreApi] = Encoder.instance {
    case x: InMemorySecretStore => Json.obj(("type", Json.string(x.getClass.getSimpleName)))
    case y: ConsulSecretStore => Json.fromFields(Seq(
      ("type", y.getClass.getSimpleName.asJson),
      ("hosts", y.consulUrls.asJson),
      ("key", y.key.asJson)))
  }
  implicit val decodeSecretStore: Decoder[SecretStoreApi] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "InMemorySecretStore" => Xor.right(defaultSecretStore)
      case "ConsulSecretStore" =>
        for {
          hosts <- c.downField("hosts").as[Set[URL]]
          key <- c.downField("key").as[String]
        } yield ConsulSecretStore(key, hosts)
      case other  => Xor.left(DecodingFailure(s"Invalid secretStore: $other", c.history))
    }
  }

  /**
   * Encoder/Decoder for protoManager
   *
   * Note that Decoder for protoManager does not work standalone, it can be only used
   * while decoding the entire ServerConfig due to dependency issues
   */
  implicit val encodeProtoManager: Encoder[ProtoManager] = Encoder.instance {
    case bpm: InternalAuthProtoManager => Json.fromFields(Seq(
      ("type", Json.string("Internal")),
      ("loginConfirm", bpm.loginConfirm.asJson),
      ("path", bpm.path.asJson),
      ("hosts", bpm.hsts.asJson)))
    case opm: OAuth2CodeProtoManager => Json.fromFields(Seq(
      ("type", Json.string("OAuth2Code")),
      ("loginConfirm", opm.loginConfirm.asJson),
      ("authorizeUrl", opm.authorizeUrl.asJson),
      ("tokenUrl", opm.tokenUrl.asJson),
      ("certificateUrl", opm.certificateUrl.asJson),
      ("clientId", opm.clientId.asJson),
      ("clientSecret", opm.clientSecret.asJson)))
  }
  implicit val decodeProtoManager: Decoder[ProtoManager] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "Internal" =>
        for {
          loginConfirm <- c.downField("loginConfirm").as[Path]
          path <- c.downField("path").as[Path]
          hosts <- c.downField("hosts").as[Set[URL]]
        } yield InternalAuthProtoManager(loginConfirm, path, hosts)
      case "OAuth2Code" =>
        for {
          loginConfirm <- c.downField("loginConfirm").as[Path]
          authorizeUrl <- c.downField("authorizeUrl").as[URL]
          tokenUrl <- c.downField("tokenUrl").as[URL]
          certificateUrl <- c.downField("certificateUrl").as[URL]
          clientId <- c.downField("clientId").as[String]
          clientSecret <- c.downField("clientSecret").as[String]
        } yield OAuth2CodeProtoManager(loginConfirm, authorizeUrl, tokenUrl, certificateUrl, clientId, clientSecret)
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
      ("identityManager", lm.identityManager.name.asJson),
      ("accessManager", lm.accessManager.name.asJson),
      ("proto", lm.protoManager.asJson)))
  }
  def decodeLoginManager(ims: Map[String, Manager], ams: Map[String, Manager]): Decoder[LoginManager] =
    Decoder.instance { c =>
      for {
        name <- c.downField("name").as[String]
        ipName <- c.downField("identityManager").as[String]
        im <- Xor.fromOption(ims.get(ipName),
          DecodingFailure(s"No IdentityManager $ipName found: ", c.history))
        apName <- c.downField("accessManager").as[String]
        am <- Xor.fromOption(ams.get(apName),
          DecodingFailure(s"No AccessManager $apName found: ", c.history)
        )
        pm <- c.downField("proto").as[ProtoManager]
      } yield LoginManager(name, im, am, pm)
    }

  // Encoder/Decoder for ServiceIdentifier
  implicit val encodeServiceIdentifier: Encoder[ServiceIdentifier] = Encoder.instance { sid =>
    Json.fromFields(Seq(
      ("name", sid.name.asJson),
      ("hosts", sid.hosts.asJson),
      ("path", sid.path.asJson),
      ("rewritePath", sid.rewritePath.asJson)))
  }
  implicit val decodeServiceIdentifier: Decoder[ServiceIdentifier] =
    Decoder.instance { c =>
      for {
        name <- c.downField("name").as[String]
        hosts <- c.downField("hosts").as[Set[URL]]
        path <- c.downField("path").as[Path]
        rewritePathOption <- c.downField("rewritePath").as[Option[Path]]
      } yield ServiceIdentifier(name, hosts, path, rewritePathOption)
    }

  // Encoder/Decoder for CustomerIdentifier
  implicit val encodeCustomerIdentifier: Encoder[CustomerIdentifier] = Encoder.instance { cid =>
    Json.fromFields(Seq(
      ("subdomain", cid.subdomain.asJson),
      ("defaultServiceIdentifier", cid.defaultServiceId.name.asJson),
      ("loginManager", cid.loginManager.name.asJson)))
  }
  def decodeCustomerIdentifier(sids: Map[String, ServiceIdentifier], lms: Map[String, LoginManager]):
      Decoder[CustomerIdentifier] =
    Decoder.instance { c =>
      for {
        subdomain <- c.downField("subdomain").as[String]
        sidName <- c.downField("defaultServiceIdentifier").as[String]
        sid <- Xor.fromOption(sids.get(sidName),
          DecodingFailure(s"No ServiceIdentifier $sidName found: ", c.history)
        )
        lmName <- c.downField("loginManager").as[String]
        lm <- Xor.fromOption(lms.get(lmName),
          DecodingFailure(s"No LoginManager $lmName found: ", c.history)
        )
      } yield CustomerIdentifier(subdomain, sid, lm)
    }

  /**
   * Decoder for ServerConfig (Using circe default encoder for encoding)
   */
  implicit val serverConfigEncoder: Encoder[ServerConfig] = Encoder.instance { serverConfig =>
    Json.fromFields(Seq(
      ("secretStore", serverConfig.secretStore.asJson),
      ("sessionStore", serverConfig.sessionStore.asJson),
      ("statsdReporter", serverConfig.statsdExporterConfig.asJson),
      ("identityManagers", serverConfig.identityManagers.asJson),
      ("accessManagers", serverConfig.accessManagers.asJson),
      ("loginManagers", serverConfig.loginManagers.asJson),
      ("serviceIdentifiers", serverConfig.serviceIdentifiers.asJson),
      ("customerIdentifiers", serverConfig.customerIdentifiers.asJson)))
  }
  implicit val serverConfigDecoder: Decoder[ServerConfig] = Decoder.instance { c =>
    for {
      secretStore <- c.downField("secretStore").as[SecretStoreApi]
      sessionStore <- c.downField("sessionStore").as[SessionStore]
      statsdExporterConfig <- c.downField("statsdReporter").as[StatsdExporterConfig]
      ims <- c.downField("identityManagers").as[Set[Manager]]
      ams <- c.downField("accessManagers").as[Set[Manager]]
      lms <- c.downField("loginManagers").as(Decoder.decodeSet(
        decodeLoginManager(ims.map(im => im.name -> im).toMap, ams.map(am => am.name -> am).toMap)))
      sids <- c.downField("serviceIdentifiers").as[Set[ServiceIdentifier]]
      cids <- c.downField("customerIdentifiers").as(Decoder.decodeSet(
        decodeCustomerIdentifier(sids.map(sid => sid.name -> sid).toMap, lms.map(lm => lm.name -> lm).toMap)))
    } yield ServerConfig(secretStore, sessionStore, statsdExporterConfig, cids, sids, lms, ims, ams)
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
   * Validate SecretStore configuration
   * @param field
   * @param secretStores
   */
  def validateSecretStoreConfig(field: String, secretStores: SecretStoreApi): Unit = {
    secretStores match {
      case x: ConsulSecretStore => validateHostsConfig(field, "consulSecretStore", x.consulUrls)
      case _ =>
    }
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
   * Validate ProtoManager configuration
   * @param field
   * @param lmName
   * @param protoManager
   */
  def validateProtoManagerConfig(field: String, lmName: String, protoManager: ProtoManager): Unit = {
    protoManager match {
      case ipm: InternalAuthProtoManager => validateHostsConfig(field, lmName, protoManager.hosts)
      case opm: OAuth2CodeProtoManager =>
    }
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
    loginManagers.map(lm => validateProtoManagerConfig(field, lm.name, lm.protoManager))
  }

  /**
   * Validate serviceIdentifier configuration
   * @param field
   * @param sids
   */
  def validateServiceIdentifierConfig(field: String, sids: Set[ServiceIdentifier]): Unit = {
    // Find if ServiceIdentifiers have duplicate entries (same name)
    if (sids.size > sids.map(sid => sid.name).size)
    throw new DuplicateConfigError("name", "serviceIdentifiers")

    // Find if ServiceIdentifiers have duplicate entries (same path)
    if (sids.size > sids.map(sid => sid.path).size)
      throw new DuplicateConfigError("path", "serviceIdentifiers")

    // Make sure hosts in Serviceidentifier have http or https protocol
    sids.map(sid => validateHostsConfig(field, sid.name, sid.hosts))
  }

  /**
   * Validate customerIdentifier configuration
   * @param field
   * @param cids
   */
  def validateCustomerIdentifierConfig(field: String, cids: Set[CustomerIdentifier]): Unit = {
    // Find if CustomerIdentifiers have duplicate entries
    if (cids.size > cids.map(cid => cid.subdomain).size)
      throw new DuplicateConfigError("subdomain", "customerIdentifiers")
  }

  /**
   * Validates the BorderPatrol Configuration
   * - for duplicates
   *
   * @param serverConfig
   */
  def validate(serverConfig: ServerConfig): Unit = {
    //  Validate Secret Store config
    validateSecretStoreConfig("secretStore", serverConfig.secretStore)

    //  Validate identityManagers config
    validateManagerConfig("identityManagers", serverConfig.identityManagers)

    //  Validate accessManagers config
    validateManagerConfig("accessManagers", serverConfig.accessManagers)

    //  Validate loginManagers config
    validateLoginManagerConfig("loginManagers", serverConfig.loginManagers)

    //  Validate serviceIdentifiers config
    validateServiceIdentifierConfig("serviceIdentifiers", serverConfig.serviceIdentifiers)

    //  Validate customerIdentifiers config
    validateCustomerIdentifierConfig("customerIdentifiers", serverConfig.customerIdentifiers)
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

