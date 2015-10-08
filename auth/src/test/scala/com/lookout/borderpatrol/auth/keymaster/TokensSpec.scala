package com.lookout.borderpatrol.auth.keymaster

import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.test.BorderPatrolSuite
import scala.util.{Success, Try}

class TokensSpec extends BorderPatrolSuite  {
  import Tokens._

  val sessionStore = SessionStores.InMemoryStore
  val serviceToken1 = new ServiceToken("SomeServiceTokenData1")
  val serviceToken2 = new ServiceToken("SomeServiceTokenData2")
  val serviceTokens = new ServiceTokens().add("service1", serviceToken1).add("service2", serviceToken2)
  val tokens = Tokens(MasterToken("masterT"), serviceTokens)
  val emptyTokens = Tokens(MasterToken(""), ServiceTokens())

  behavior of "ServiceTokens"

  it should "Verify the ServiceTokens methods" in {
    serviceTokens.find("service1") should be equals (serviceToken1)
    serviceTokens.find("service2") should be equals (serviceToken2)
  }

  it should "Verify that encode and decode for ServiceTokens" in {
    def encodeDecode(tokens: ServiceTokens) : ServiceTokens = {
      ServiceTokensDecoder.decodeJson(ServiceTokensEncoder(tokens)).fold[ServiceTokens](e => ServiceTokens(), t => t)
    }

    encodeDecode(serviceTokens) should be (serviceTokens)
  }

  behavior of "Tokens"

  it should "Verify the Tokens methods" in {
    tokens.service("service1") should be equals (serviceToken1)
    tokens.service("service2") should be equals (serviceToken2)
    tokens.service("service3") should be equals (None)
  }

  it should "Verify the encode and decode for Tokens" in {
    def encodeDecode(toks: Tokens) : Tokens =
      TokensDecoder.decodeJson(TokensEncoder(toks)).fold[Tokens](e => emptyTokens, t => t)
    encodeDecode(tokens) should be (tokens)
  }

  behavior of "SessionDataTokenEncoder"

  it should "Verify encode and decode" in {
    def validate[A](s: Tokens)(implicit ev: SessionDataEncoder[Tokens]): Try[Tokens] = {
      ev.decode(ev.encode(s))
    }

    // Validate
    validate(tokens) should be (Success(tokens))
  }
}