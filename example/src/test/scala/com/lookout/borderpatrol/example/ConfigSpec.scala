package com.lookout.borderpatrol.example

import java.net.{URI, URL}

import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.{LoginManager, Manager, ServiceMatcher, ServiceIdentifier}
import com.twitter.finagle.MemcachedClient
import com.twitter.finagle.httpx.path.Path
import org.scalatest.{OptionValues, TryValues, Matchers, FlatSpec}
import cats.data.Xor
import io.circe._
import io.circe.jawn._
import io.circe.generic.auto._
import io.circe.syntax._
import scala.reflect.io.File


class ConfigSpec extends FlatSpec with Matchers with TryValues with OptionValues {
  import Config._

  val urls = Set(new URL("http://localhost:8081"))
  val keymasterIdManager = Manager("keymaster", Path("/identityProvider"), urls)
  val keymasterAccessManager = Manager("keymaster", Path("/accessIssuer"), urls)
  val checkpointLoginManager = LoginManager("checkpoint", Path("/check"), urls, Path("/loginConfirm"),
    keymasterIdManager, keymasterAccessManager)

  val basicIdManager = Manager("basic", Path("/signin"), urls)
  val basicAccessManager = Manager("basic", Path("/accessin"), urls)
  val umbrellaLoginManager = LoginManager("umbrella", Path("/umb"), urls, Path("/loginIt"),
    keymasterIdManager, keymasterAccessManager)

  val one = ServiceIdentifier("one", urls, Path("/ent"), "enterprise", checkpointLoginManager)
  val two = ServiceIdentifier("two", urls, Path("/api"), "api", umbrellaLoginManager)
  val three = ServiceIdentifier("three", urls, Path("/apis"), "api.subdomain", checkpointLoginManager)
  val four = ServiceIdentifier("four", urls, Path("/apis/test"), "api.testdomain", umbrellaLoginManager)
  val sids = Set(one, two, three, four)
  val serviceMatcher = ServiceMatcher(sids)

  val defaultSecretStore = SecretStores.InMemorySecretStore(Secrets(Secret(), Secret()))
  val defaultSessionStore = SessionStores.InMemoryStore
  val memcachedSessionStore = SessionStores.MemcachedStore(MemcachedClient.newRichClient("localhost:1234"))
  val serverConfig = ServerConfig(defaultSecretStore, defaultSessionStore, sids,
    Set(checkpointLoginManager, umbrellaLoginManager), Set(keymasterIdManager), Set(keymasterAccessManager))
  val serverConfig1 = ServerConfig(defaultSecretStore, memcachedSessionStore, sids,
    Set(checkpointLoginManager, umbrellaLoginManager), Set(keymasterIdManager), Set(keymasterAccessManager))

  def verifyServerConfig(a: ServerConfig, b: ServerConfig): Unit = {
    a.secretStore.getClass should be (b.secretStore.getClass)
    a.sessionStore.getClass should be (b.sessionStore.getClass)
    assert(a.serviceIdentifiers == b.serviceIdentifiers)
    assert(a.loginManagers == b.loginManagers)
    assert(a.identityManagers == b.identityManagers)
    assert(a.accessManagers == b.accessManagers)
  }

  behavior of "Config"

  it should "uphold encoding/decoding Manager" in {
    def encodeDecode(m: Manager) : Manager = {
      val encoded = m.asJson
      decode[Manager](encoded.toString()) match {
        case Xor.Right(a) => a
        case Xor.Left(b) => Manager("failed", Path("f"), urls)
      }
    }
    encodeDecode(keymasterIdManager) should be (keymasterIdManager)
  }

  it should "uphold encoding/decoding ServerConfig" in {
    def encodeDecode(config: ServerConfig) : ServerConfig = {
      val encoded = config.asJson
      decode[ServerConfig](encoded.toString()) match {
        case Xor.Right(a) => a
        case Xor.Left(b) => ServerConfig(defaultSecretStore, defaultSessionStore, Set(), Set(), Set(), Set())
      }
    }
    val foo = encodeDecode(serverConfig)
    verifyServerConfig(encodeDecode(serverConfig), serverConfig)
    verifyServerConfig(encodeDecode(serverConfig1), serverConfig1)
  }

  it should "succeed to build a valid ServerConfig from a file with valid contents" in {
    val validContents = serverConfig.asJson.toString()
    val tempValidFile = File.makeTemp("ServerConfigValid", ".tmp")
    tempValidFile.writeAll(validContents)

    val readConfig = readServerConfig(tempValidFile.toCanonical.toString)
    verifyServerConfig(readConfig, serverConfig)
  }

  it should "fail and raise an exception while reading from a file with invalid contents" in {
    val invalidContents = """[{"name":"one","path": {"str" :"customer1"},"subdomain":"customer1","login":"/login"}]"""
    val tempInvalidFile = File.makeTemp("ServerConfigSpecInvalid", ".tmp")
    tempInvalidFile.writeAll(invalidContents)
    val uri = new URI("http://foo.com")
    val caught = the [ConfigError] thrownBy {
      readServerConfig(tempInvalidFile.toCanonical.toString)
    }
    caught.getMessage should include regex ("Failed to decode following fields: secretStore")
  }

  it should "raise a ConfigError exception due to lack of Secret Store config" in {
    val partialContents = Json.fromFields(Seq(
      ("sessionStore", defaultSessionStore.asInstanceOf[SessionStore].asJson),
      ("serviceIdentifiers", sids.asJson),
      ("loginManagers", Set(checkpointLoginManager).asJson),
      ("identityManagers", Set(keymasterIdManager).asJson),
      ("accessManagers", Set(keymasterAccessManager).asJson)))

    val tempFile = File.makeTemp("ServerConfigTest", ".tmp")
    tempFile.writeAll(partialContents.toString)

    val caught = the [ConfigError] thrownBy {
      readServerConfig(tempFile.toCanonical.toString)
    }
    caught.getMessage should include regex ("Failed to decode following fields: secretStore")
  }

  it should "raise a ConfigError exception due to invalid of Secret Store config" in {
    val partialContents = Json.fromFields(Seq(
      ("secretStore", Json.obj(("type", Json.string("woof")))),
      ("sessionStore", defaultSessionStore.asInstanceOf[SessionStore].asJson),
      ("serviceIdentifiers", sids.asJson),
      ("loginManagers", Set(checkpointLoginManager).asJson),
      ("identityManagers", Set(keymasterIdManager).asJson),
      ("accessManagers", Set(keymasterAccessManager).asJson)))

    val tempFile = File.makeTemp("ServerConfigTest", ".tmp")
    tempFile.writeAll(partialContents.toString)

    val caught = the [ConfigError] thrownBy {
      readServerConfig(tempFile.toCanonical.toString)
    }
    caught.getMessage should include ("Failed to decode following fields: secretStore")
  }

  it should "raise a ConfigError exception due to lack of Session Store config" in {
    val partialContents = Json.fromFields(Seq(
      ("secretStore", defaultSecretStore.asInstanceOf[SecretStoreApi].asJson),
      ("serviceIdentifiers", sids.asJson),
      ("loginManagers", Set(checkpointLoginManager).asJson),
      ("identityManagers", Set(keymasterIdManager).asJson),
      ("accessManagers", Set(keymasterAccessManager).asJson)))

    val tempFile = File.makeTemp("ServerConfigTest", ".tmp")
    tempFile.writeAll(partialContents.toString)

    val caught = the [ConfigError] thrownBy {
      readServerConfig(tempFile.toCanonical.toString)
    }
    caught.getMessage should include regex ("Failed to decode following fields: sessionStore")
  }

  it should "raise a ConfigError exception due to invalid Session Store config" in {
    val partialContents = Json.fromFields(Seq(
      ("secretStore", defaultSecretStore.asInstanceOf[SecretStoreApi].asJson),
      ("sessionStore", Json.obj(("type", Json.string("woof")))),
      ("serviceIdentifiers", sids.asJson),
      ("loginManagers", Set(checkpointLoginManager).asJson),
      ("identityManagers", Set(keymasterIdManager).asJson),
      ("accessManagers", Set(keymasterAccessManager).asJson)))

    val tempFile = File.makeTemp("ServerConfigTest", ".tmp")
    tempFile.writeAll(partialContents.toString)

    val caught = the [ConfigError] thrownBy {
      readServerConfig(tempFile.toCanonical.toString)
    }
    caught.getMessage should include ("Failed to decode following fields: sessionStore")
  }

  it should "raise a ConfigError exception due to lack of ServiceIdentifier config" in {
    val partialContents = Json.fromFields(Seq(
      ("secretStore", defaultSecretStore.asInstanceOf[SecretStoreApi].asJson),
      ("sessionStore", defaultSessionStore.asInstanceOf[SessionStore].asJson),
      ("loginManagers", Set(checkpointLoginManager).asJson),
      ("identityManagers", Set(keymasterIdManager).asJson),
      ("accessManagers", Set(keymasterAccessManager).asJson)))

    val tempFile = File.makeTemp("ServerConfigTest", ".tmp")
    tempFile.writeAll(partialContents.toString)

    val caught = the [ConfigError] thrownBy {
      readServerConfig(tempFile.toCanonical.toString)
    }
    caught.getMessage should include regex ("Failed to decode following fields: serviceIdentifiers")
  }

  it should "raise a ConfigError exception if identityManager that is used in LoginManager is missing" in {
    val partialContents = Json.fromFields(Seq(
      ("secretStore", defaultSecretStore.asInstanceOf[SecretStoreApi].asJson),
      ("sessionStore", defaultSessionStore.asInstanceOf[SessionStore].asJson),
      ("serviceIdentifiers", sids.asJson),
      ("loginManagers", Set(checkpointLoginManager).asJson),
      ("identityManagers", Set(Manager("some", Path("/some"), urls)).asJson),
      ("accessManagers", Set(keymasterAccessManager).asJson)))

    val tempFile = File.makeTemp("ServerConfigTest", ".tmp")
    tempFile.writeAll(partialContents.toString)

    val caught = the [ConfigError] thrownBy {
      readServerConfig(tempFile.toCanonical.toString)
    }
    caught.getMessage should include regex ("Failed to decode following fields: loginManagers")
  }

  it should "raise a ConfigError exception if duplicate are configured in idManagers config" in {
    val partialContents = Json.fromFields(Seq(
      ("secretStore", defaultSecretStore.asInstanceOf[SecretStoreApi].asJson),
      ("sessionStore", defaultSessionStore.asInstanceOf[SessionStore].asJson),
      ("serviceIdentifiers", sids.asJson),
      ("loginManagers", Set(checkpointLoginManager, umbrellaLoginManager).asJson),
      ("identityManagers", Set(keymasterIdManager,
        Manager("keymaster", Path("/some"), urls)).asJson),
      ("accessManagers", Set(keymasterAccessManager).asJson)))

    val tempFile = File.makeTemp("ServerConfigTest", ".tmp")
    tempFile.writeAll(partialContents.toString)

    val caught = the [DuplicateConfigError] thrownBy {
      readServerConfig(tempFile.toCanonical.toString)
    }
    caught.getMessage should include ("Duplicate entries for key(s) (name) - are found in the field: identityManagers")
  }

  it should "raise a ConfigError exception if duplicates are configured in accessManagers config" in {
    val partialContents = Json.fromFields(Seq(
      ("secretStore", defaultSecretStore.asInstanceOf[SecretStoreApi].asJson),
      ("sessionStore", defaultSessionStore.asInstanceOf[SessionStore].asJson),
      ("serviceIdentifiers", sids.asJson),
      ("loginManagers", Set(checkpointLoginManager, umbrellaLoginManager).asJson),
      ("identityManagers", Set(keymasterIdManager).asJson),
      ("accessManagers", Set(keymasterAccessManager,
        Manager("keymaster", Path("/some"), urls)).asJson)))

    val tempFile = File.makeTemp("ServerConfigTest", ".tmp")
    tempFile.writeAll(partialContents.toString)

    val caught = the [DuplicateConfigError] thrownBy {
      readServerConfig(tempFile.toCanonical.toString)
    }
    caught.getMessage should include ("Duplicate entries for key(s) (name) - are found in the field: accessManagers")
  }

  it should "raise a ConfigError exception if duplicates are configured in loginManagers config" in {
    val partialContents = Json.fromFields(Seq(
      ("secretStore", defaultSecretStore.asInstanceOf[SecretStoreApi].asJson),
      ("sessionStore", defaultSessionStore.asInstanceOf[SessionStore].asJson),
      ("serviceIdentifiers", sids.asJson),
      ("loginManagers", Set(checkpointLoginManager, umbrellaLoginManager,
        LoginManager("checkpoint", Path("/some"), urls, Path("/some"),
          keymasterIdManager, keymasterAccessManager)).asJson),
      ("identityManagers", Set(keymasterIdManager).asJson),
      ("accessManagers", Set(keymasterAccessManager).asJson)))

    val tempFile = File.makeTemp("ServerConfigTest", ".tmp")
    tempFile.writeAll(partialContents.toString)

    val caught = the [DuplicateConfigError] thrownBy {
      readServerConfig(tempFile.toCanonical.toString)
    }
    caught.getMessage should include ("Duplicate entries for key(s) (name) - are found in the field: loginManagers")
  }

  it should "raise a ConfigError exception if duplicates are configured in serviceIdentifiers config" in {
    val partialContents = Json.fromFields(Seq(
      ("secretStore", defaultSecretStore.asInstanceOf[SecretStoreApi].asJson),
      ("sessionStore", defaultSessionStore.asInstanceOf[SessionStore].asJson),
      ("serviceIdentifiers", (sids + ServiceIdentifier("some", urls, Path("/ent"), "enterprise",
        checkpointLoginManager)).asJson),
      ("loginManagers", Set(checkpointLoginManager, umbrellaLoginManager).asJson),
      ("identityManagers", Set(keymasterIdManager).asJson),
      ("accessManagers", Set(keymasterAccessManager).asJson)))

    val tempFile = File.makeTemp("ServerConfigTest", ".tmp")
    tempFile.writeAll(partialContents.toString)

    val caught = the [DuplicateConfigError] thrownBy {
      readServerConfig(tempFile.toCanonical.toString)
    }
    caught.getMessage should include (
      "Duplicate entries for key(s) (path and subdomain) - are found in the field: serviceIdentifiers")
  }

  it should "validate URLs configuration" in {
    val u1 = new URL("http://sample.com")
    val u2 = new URL("http://sample.com:8080/goto")
    val u3 = new URL("http://tample.com:2345/foo")
    val u4 = new URL("https://xample.com:2222")
    val u5 = new URL("https://ample.com:2221")
    val u11 = new URL("ftp://localhost:123")

    validateHostsConfig("some", "working1", Set(u1, u2))
    validateHostsConfig("some", "working2", Set(u4))
    val caught1 = the [InvalidConfigError] thrownBy {
      validateHostsConfig("some", "failed1", Set(u3, u4))
    }
    caught1.getMessage should include ("hosts configuration for failed1 in some: has differing protocols")
    val caught2 = the [InvalidConfigError] thrownBy {
      validateHostsConfig("some", "failed2", Set(u11))
    }
    caught2.getMessage should include ("hosts configuration for failed2 in some: has unsupported protocol")
    val caught3 = the [InvalidConfigError] thrownBy {
      validateHostsConfig("some", "failed3", Set(u4, u5))
    }
    caught3.getMessage should include ("hosts configuration for failed3 in some: https urls have mismatching hostnames")
  }
}
