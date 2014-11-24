package com.lookout.borderpatrol.session

import argonaut._
import Argonaut._

sealed trait Token {
  val value: String
}

case class MasterToken(value: String) extends Token
case class ServiceToken(name: String, value: String) extends Token

sealed trait SessionTokens {
  val master: MasterToken
  def service(name: String): Option[ServiceToken]
  def +(token: ServiceToken): SessionTokens
}

object SessionTokens {
  def apply(master: String, services: Option[Map[String, String]]): SessionTokens = services match {
    case Some(st) => Tokens(MasterToken(master), ServiceTokensMap(st))
    case _ => MasterTokenOnly(MasterToken(master))
  }
}

trait ServiceTokens {
  def get(name: String): Option[ServiceToken]
  def +(st: ServiceToken): ServiceTokens
}

case class ServiceTokensMap(map: Map[String, String]) extends ServiceTokens {
  def get(name: String) =
    map.get(name).map(v => ServiceToken(name, v))
  def +(st: ServiceToken): ServiceTokensMap =
    ServiceTokensMap(map + (st.name -> st.value))
}

case class ServiceTokensSet(set: Set[ServiceToken]) extends ServiceTokens {
  def get(name: String) =
    set.find(_.name == name)
  def +(st: ServiceToken): ServiceTokensSet =
    ServiceTokensSet(set + st)
}

case class MasterTokenOnly(master: MasterToken) extends SessionTokens {
  val services = None
  def service(name: String) =
    None
  def +(token: ServiceToken): Tokens =
    Tokens(master, ServiceTokensMap(Map[String, String](token.name -> token.value)))
}

case class Tokens(master: MasterToken, services: ServiceTokensMap) extends SessionTokens {
  def service(name: String) =
    services get name
  def +(token: ServiceToken): Tokens =
    Tokens(master, services + token)
}


object TokenJson {
  val i = """{"auth_service": "abcd", "service_tokens": {"service1": "jk", "service2": "ep"}}"""

  implicit def MTokenCodecJson: CodecJson[MasterToken] = casecodec1(MasterToken.apply, MasterToken.unapply)("auth_service")
  implicit def STokensMapCodeJson: CodecJson[ServiceTokensMap] = casecodec1(ServiceTokensMap.apply, ServiceTokensMap.unapply)("service_tokens")

  /*
  implicit def TokensCodecJson: CodecJson[Tokens] =
    CodecJson(
      (ts: Tokens) =>
        ("auth_service" := ts.master) ->:
        ("service_tokens" := ts.services) ->:
          jEmptyObject,
      c => for {
        master <- (c --\ "auth_service").as[MasterToken]
        services <- (c --\ "service_tokens").as[ServiceTokensMap]
      } yield Tokens(master, services))

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
    */
}
