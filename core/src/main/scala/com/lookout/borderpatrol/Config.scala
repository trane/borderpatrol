package com.lookout.borderpatrol

import com.lookout.borderpatrol.sessionx.SecretStores.InMemorySecretStore
import com.lookout.borderpatrol.sessionx.SessionStores.{MemcachedStore, InMemoryStore}
import com.lookout.borderpatrol.sessionx._
import com.twitter.finagle.MemcachedClient
import com.twitter.finagle.httpx.path.Path
import com.twitter.app.{Flaggable, App}
import cats.data.Xor
import io.circe.{Encoder, _}
import io.circe.jawn._
import io.circe.generic.auto._ // DO NOT REMOVE
import scala.io.Source


case class ServerConfig(secretStore: SecretStoreApi,
                        sessionStore: SessionStore,
                        serviceIdentifiers: Set[ServiceIdentifier])

class ConfigError(val message: String) extends Exception(message, null)

/**
 * Where you will find the Secret Store and Session Store
 */
object Config {

  def defaultSecretStore = SecretStores.InMemorySecretStore(Secrets(Secret(), Secret()))
  def defaultSessionStore = SessionStores.InMemoryStore
  def defaultServiceIdsFile = "bpSids.json"

  // Flaggable for Secret Store
  implicit val secretStoreApiFlaggable = new Flaggable[SecretStoreApi] {
    def parse (s: String): SecretStoreApi = {
      throw new ConfigError("Invalid Secret Store configuration")
    }
    override def show(t: SecretStoreApi) = t match {
      case InMemorySecretStore(_) => "In Memory Secret Store"
      case a => a.getClass.getSimpleName
    }
  }

  // Flaggable for Session Store
  implicit val sessionStoreApiFlaggable = new Flaggable[SessionStore] {
    def parse (s: String): SessionStore = {
      println("Inside sessionStoreApiFlaggable: " + s)
      SessionStores.MemcachedStore(MemcachedClient.newRichClient(s))
    }
    override def show(t: SessionStore) = t match {
      case InMemoryStore => "In Memory Session Store"
      case MemcachedStore(_) => "Memcached Session Store"
      case a => a.getClass.getSimpleName
    }
  }

  // Flaggable for ServiceIds
  implicit val serviceIdsFlaggable = new Flaggable[Set[ServiceIdentifier]] {
    def parse (s: String): Set[ServiceIdentifier] = {

      // Set of ServiceIdentifier
      implicit val encodePath: Encoder[Path] = Encoder.instance(p => Json.obj( ("str", Json.string(p.toString())) ))
      implicit val decodePath: Decoder[Path] = Decoder.instance(c =>
        for {
          str <- c.downField("str").as[String]
        } yield Path(str)
      )

      decode[Set[ServiceIdentifier]](Source.fromFile(s).mkString) match {
        case Xor.Right(a) => a.asInstanceOf[Set[ServiceIdentifier]]
        case Xor.Left(b) => throw new ConfigError(b.getClass.getSimpleName + ": " + b.getMessage())
      }
    }
  }
}

/**
 * A [[com.twitter.app.App]] mixin to use for Configuration. Defines flags
 * to configure the BorderPatrol Server
 */
trait Config {self: App =>
  import Config._

  println("BEfore flags")

  def defaultServiceIds = serviceIdsFlaggable.parse(defaultServiceIdsFile)

  // Flag for Secret Store
  val secretStore = flag[SecretStoreApi]("secretStore.servers", defaultSecretStore,
    "CSV of Memcached hosts for Secret Store. Default is in-memory store.")

  // Flag for Session Store
  val sessionStore = flag[SessionStore]("sessionStore.servers", defaultSessionStore,
    "CSV of Memcached hosts for Session Store. Default is in-memory store.")

  val serviceIds = flag[Set[ServiceIdentifier]]("serviceIds.file",
    defaultServiceIds,
    "Filename to read Service Identifiers in JSON format")
}

