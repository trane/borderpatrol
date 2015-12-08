package com.lookout.borderpatrol.auth.keymaster

import com.lookout.borderpatrol.auth.TokenParsingError
import com.lookout.borderpatrol.sessionx.{SessionDataError, SessionDataEncoder}
import com.nimbusds.jwt.{PlainJWT, JWTClaimsSet}
import com.twitter.io.Buf
import com.twitter.util.Future
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
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
case class ServiceToken(value: String) extends Token {
  override def toString: String = value
}

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

/**
 * AAD token
 * @param accessToken JWT Access Token
 * @param idToken JWT ID Token
 */
case class AadToken(accessToken: String, idToken: String) extends Token

object Tokens {

  import io.circe.generic.semiauto._
  import cats.data.Xor

  def derive[A : Decoder](input: String): Xor[Error, A] =
    jawn.decode[A](input)

  /**
   * MasterToken Encoder/Decoder
   * {"auth_service": "a"} -> MasterToken("a")
   * {} -> result error
   */
  implicit val MasterTokenDecoder: Decoder[MasterToken] = Decoder[String].map(MasterToken(_))
  implicit val MasterTokenEncoder: Encoder[MasterToken] = Encoder[String].contramap(_.value)

  /**
   * Service Token Encoder/Decoder
   * {"service_name": "service_token"} -> ServiceToken("service_token")
   */
  implicit val ServiceTokenDecoder: Decoder[ServiceToken] = Decoder[String].map(ServiceToken(_))
  implicit val ServiceTokenEncoder: Encoder[ServiceToken] = Encoder[String].contramap(_.value)

  /**
   * {"service_tokens": {"a": "a", "b": "b"}} -> ServiceTokens(Map((a->ServiceToken(a)), (b->ServiceToken(b)))
   * {} -> ServiceTokens(Map())
   */
  implicit val ServiceTokensEncoder: Encoder[ServiceTokens] = Encoder.instance[ServiceTokens](st =>
    Json.fromFields(st.services.map(t => (t._1, Json.string(t._2.value))).toSeq))

  /**
   * Tokens Encoder/Decoder
   */
  implicit val TokensDecoder: Decoder[Tokens] = Decoder.instance {c =>
    for {
      master <- c.downField("auth_service").as[MasterToken]
      services <- c.downField("service_tokens").as[Option[Map[String, String]]]
    } yield Tokens(master, services.fold(ServiceTokens())(m => ServiceTokens(m.mapValues(ServiceToken(_)))))
  }

  implicit val TokensEncoder: Encoder[Tokens] = Encoder.instance {t =>
    Json.fromFields(Seq(
      ("auth_service", t.master.asJson),
      ("service_tokens", t.services.asJson)))
  }

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

  /**
   * AadToken Encoder/Decoder
   */
  implicit val AadTokenEncoder: Encoder[AadToken] = Encoder.instance {t =>
    Json.fromFields(Seq(
      ("access_token", t.accessToken.asJson),
      ("id_token", t.idToken.asJson)))
  }

  implicit val AadTokenDecoder: Decoder[AadToken] = Decoder.instance {c =>
    for {
      accessToken <- c.downField("access_token").as[String]
      idToken <- c.downField("id_token").as[String]
    } yield AadToken(accessToken, idToken)
  }

  /**
   * JWT parser wrapper
   */
  def JwtParse(tokenStr: String): Future[JWTClaimsSet] =
    wrapFuture[JWTClaimsSet]({() => PlainJWT.parse(tokenStr).getJWTClaimsSet }, TokenParsingError.apply)
}

