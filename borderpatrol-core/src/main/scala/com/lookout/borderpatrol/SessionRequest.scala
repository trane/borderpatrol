package com.lookout.borderpatrol

import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http.{Response, Request, RequestProxy}
import com.twitter.util.{Future, Time}
import scala.util.{Failure, Success, Try, Random}

/**
object Session {
  type Token = String
  type MasterToken = Token
  type ServiceToken = Token

  def thing(mt: Option[MasterToken]) = ???

}

import argonaut._
import Argonaut._

object Stuff {
  type Token = String
  type Service = String

  val i = """{"auth_service": "abcd", "service_tokens": {"service1": "jk", "service2": "ep"}}"""

  case class SToken(service: Service, token: Token)

  case class STokenMap(service_tokens: Map[String, Token])

  case class MToken(auth_service: Token)

  case class Tokens(auth_service: String, service_tokens: Map[String, String]) {
    // def apply(auth_service: String, service_tokens: Map[String, String]) = {
    //   SessionTokens(MToken(auth_service), (service_tokens map (t => SToken(t._1, t._2))).toSet
    // }
  }

  case class SessionTokens(masterToken: MToken, serviceTokens: Set[SToken])

  case class STokenSet(service_tokens: Set[SToken])

  implicit def MTokenCodecJson: CodecJson[MToken] = casecodec1(MToken.apply, MToken.unapply)("auth_service")

  implicit def STokenMapCodecJson: CodecJson[STokenMap] =
    casecodec1(STokenMap.apply, STokenMap.unapply)("service_tokens")

  implicit def TokensCodecJson: CodecJson[Tokens] = casecodec2(Tokens.apply, Tokens.unapply)("auth_service", "service_tokens")

  implicit def TokensCodecJson: CodecJson[Tokens] =
    CodecJson(
      (t: Tokens) =>
        ("auth_service" := t.auth_service) ->:
          ("service_tokens" := t.service_tokens) ->:
          jEmptyObject,
      c => for {
        as <- (c --\ "auth_service").as[String]
        st <- (c --\ "service_tokens").as[Map[String, String]]
      } yield Tokens(as, st))

  i.decodeOption[Tokens]

  implicit def STokenCodecJson: CodecJson[SToken] =
    CodecJson(
      (st: SToken) =>
        (st.service := st.token) ->:
          jEmptyObject,
      c => for {
        m <- (c --\ jString).as[Map[String, String]]
        (k, v) <- m
      } yield SToken(k, v))

  implicit def STokenMapEncodeJson: EncodeJson[STokenMap] =
    EncodeJson((stm: STokenMap) => ("service_tokens" := stm.service_tokens)) ->: jEmptyObject

  implicit def STokenMapDecodeJson: DecodeJson[STokenMap] =
    DecodeJson(c => for {
      mp <- (c --\ "service_tokens").as[STokenMap]
    } yield mp)
}

*/
