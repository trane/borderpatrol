package com.lookout.borderpatrol.session

import argonaut._
import Argonaut._

package object tokens {

  implicit def MasterTokenCodecJson: CodecJson[MasterToken] =
    casecodec1(MasterToken.apply, MasterToken.unapply)("value")

  implicit def ServiceTokenCodecJson: CodecJson[ServiceToken] =
    casecodec2(ServiceToken.apply, ServiceToken.unapply)("name", "value")

  implicit def ServiceTokensCodecJson: CodecJson[ServiceTokens] =
    casecodec1(ServiceTokens.apply, ServiceTokens.unapply)("service_tokens")

  implicit def TokensCodecJson: CodecJson[Tokens] =
    CodecJson(
      (ts: Tokens) =>
        ("auth_service" := ts.master.value) ->:
          ("service_tokens" := ts.services.services) ->:
          jEmptyObject,
      c => for {
        master <- (c --\ "auth_service").as[String]
        services <- (c --\ "service_tokens").as[Map[String, String]]
      } yield Tokens(if (master.value.isEmpty) EmptyToken else MasterToken(master),
          if (services.isEmpty) EmptyServiceTokens else ServiceTokens(services)))

  implicit class TokensJsonEncode(val t: Tokens) extends AnyVal {
    def asJson: String =
      TokensCodecJson.encode(t).toString
  }

  implicit class MasterTokenJsonEncode(val t: MasterToken) extends AnyVal {
    def asJson: String =
      MasterTokenCodecJson.encode(t).toString
  }

  implicit class ServiceTokenJsonEncode(val t: ServiceToken) extends AnyVal {
    def asJson: String =
      ServiceTokenCodecJson.encode(t).toString
  }

  implicit class ServiceTokensJsonEncode(val t: ServiceTokens) extends AnyVal {
    def asJson: String =
      ServiceTokensCodecJson.encode(t).toString
  }

  implicit class StringOpsTokens(val s: String) extends AnyVal {

    def asMasterToken: Option[MasterToken] =
      s.decodeOption[MasterToken]

    def asServiceToken: Option[ServiceToken] =
      s.decodeOption[ServiceToken]

    def asServiceTokens: Option[ServiceTokens] =
      s.decodeOption[ServiceTokens]

    def asTokens: Option[Tokens] =
      s.decodeOption[Tokens]
  }

}
