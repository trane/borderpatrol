package com.lookout.borderpatrol.session

import argonaut._
import Argonaut._
import org.jboss.netty.buffer.ChannelBuffer

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
    case Some(s) => Tokens(MasterToken(master), ServiceTokensMap(s))
    case None => Tokens(MasterToken(master), ServiceTokensMap())
  }
}

trait ServiceTokens {
  def get(name: String): Option[ServiceToken]
  def +(st: ServiceToken): ServiceTokens
}

case class ServiceTokensMap(map: Map[String, String] = Map[String, String]()) extends ServiceTokens {
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

case class TokensOption(master: MasterToken, services: Option[ServiceTokensMap]) extends SessionTokens {
  def service(name: String) =
    services.flatMap(_ get name)
  def +(token: ServiceToken): Tokens =
    Tokens(master, services.map(_ + token).getOrElse(ServiceTokensMap()))
}

case class Tokens(master: MasterToken, services: ServiceTokensMap) extends SessionTokens {
  def service(name: String) =
    services get name
  def +(token: ServiceToken): Tokens =
    Tokens(master, services + token)
}

object TokenJson {
  implicit def TokensCodecJson: CodecJson[Tokens] =
    CodecJson(
      (ts: Tokens) =>
        ("auth_service" := ts.master) ->:
        ("service_tokens" := ts.services) ->:
          jEmptyObject,
      c => for {
        master <- (c --\ "auth_service").as[String]
        services <- (c --\ "service_tokens").as[Map[String, String]]
      } yield Tokens(MasterToken(master), ServiceTokensMap(services)))

  def apply(s: String) =
    s.decodeOption[Tokens]
  def apply(b: ChannelBuffer) =
    b.toString.decodeOption[Tokens]
}
