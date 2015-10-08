package com.lookout.borderpatrol.auth.keymaster

import com.lookout.borderpatrol.sessionx.{SessionDataError, SessionDataEncoder}
import com.twitter.io.Buf
import io.circe._
import io.circe.generic.auto._
import scala.util.{Try, Success, Failure}


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

  def add(name: String, token: ServiceToken): ServiceTokens =
    copy(services = (this.services + ((name, token))))
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

  def add(name: String, serviceToken: ServiceToken): Tokens =
    copy(services = this.services.add(name, serviceToken))
}

object Tokens {

  import io.circe.generic.semiauto._
  import cats.data.Xor

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

  implicit val MasterTokenEncoder: Encoder[MasterToken] = Encoder.instance[MasterToken](t =>
    Json.obj(("auth_service", Json.string(t.value)))
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

  implicit val ServiceTokensEncoder: Encoder[ServiceTokens] = Encoder.instance[ServiceTokens](st =>
    Json.obj(("service_tokens", Json.fromFields(st.services.map(t => (t._1, Json.string(t._2.value))).toSeq)))
  )

  implicit val TokensDecoder: Decoder[Tokens] = deriveFor[Tokens].decoder

  implicit val TokensEncoder: Encoder[Tokens] = deriveFor[Tokens].encoder

  implicit val SessionDataTokenEncoder: SessionDataEncoder[Tokens] = new SessionDataEncoder[Tokens] {
    def encode(tokens: Tokens): Buf =
      SessionDataEncoder.encodeString.encode(TokensEncoder(tokens).toString())

    def decode(buf: Buf): Try[Tokens] =
      SessionDataEncoder.encodeString.decode(buf).flatMap(s =>
        derive[Tokens](s).fold[Try[Tokens]](
          e => Failure(SessionDataError(e)),
          t => Success(t)
        )
      )
  }
}

