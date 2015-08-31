package com.lookout.borderpatrol.auth.keymaster

import com.twitter.finagle.httpx.Response
import io.circe._
import cats.data.Xor

/**
 * A Token is an abstraction for the opaque string value for the JSON map that Keymaster returns
 */
sealed trait Token

/**
 * The token is missing
 */
case object EmptyToken extends Token

/**
 * Primary identifier, used to re-ask the issuing service for a service access token
 */
case class MasterToken(value: String) extends Token

/**
 * Service access token to be injected into request to service
 */
case class ServiceToken(value: String) extends Token

/**
 * A mapping of service names to service tokens
 */
case class ServiceTokens(services: Map[String, ServiceToken] = Map.empty[String, ServiceToken]) {
  def find(name: String): Option[ServiceToken] =
    services.get(name)
}

/**
 * This is the primary interface for accessing tokens
 * The incoming format from Keymaster is like this:
 * {
 *  "auth_service" : "MMM",
 *  "service_tokens" : { "service_a": "AAA", "service_b": "BBB" }
 * }
 *
 * @example
 *         Tokens.empty.service("service_name") // returns None
 *         decode[Tokens](jsonString) // result in circe decode result
 */
case class Tokens(master: MasterToken, services: ServiceTokens) {
  def service(name: String): Option[ServiceToken] =
    services.find(name)
}

object Tokens {

  def derive[A : Decoder](input: String): Xor[Error, A] =
    jawn.decode[A](input)

  /**
   * {"auth_service": "a"} -> MasterToken("a")
   * {} -> result error
   */
  implicit val MasterTokenDecoder: Decoder[MasterToken] = Decoder.instance[MasterToken]( c =>
    for {
      value <- (c.downField("auth_service")).as[String]
    } yield MasterToken(value)
  )

  /**
   * {"service_tokens": {"a": "a", "b": "b"}} -> ServiceTokens(Map((a->ServiceToken(a)), (b->ServiceToken(b)))
   * {} -> ServiceTokens(Map())
   */
  implicit val ServiceTokensDecoder: Decoder[ServiceTokens] = Decoder.instance[ServiceTokens](c =>
    for {
      services <- c.downField("service_tokens").as[Option[Map[String, String]]]
    } yield services.fold(ServiceTokens())(m => ServiceTokens(m.mapValues(ServiceToken(_))))
  )

  implicit val TokensDecoder: Decoder[Tokens] = Decoder.instance[Tokens](c =>
    for {
      master <- MasterTokenDecoder(c)
      services <- ServiceTokensDecoder(c)
    } yield Tokens(master, services)
  )
}

