package com.lookout.borderpatrol.auth.keymaster

import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.test.BorderPatrolSuite
import com.twitter.io.Buf
import scala.util.{Try, Success}

class TokensSpec extends BorderPatrolSuite  {
  import Tokens._

  val sessionStore = SessionStores.InMemoryStore
  val serviceToken1 = new ServiceToken("SomeServiceTokenData1")
  val serviceToken2 = new ServiceToken("SomeServiceTokenData2")
  val serviceTokens = new ServiceTokens().add("service1", serviceToken1).add("service2", serviceToken2)
  val tokens = Tokens(MasterToken("masterT"), serviceTokens)
  val emptyTokens = Tokens(MasterToken(""), ServiceTokens())

  behavior of "ServiceTokens"

  it should "be able to find ServiceToken by service name" in {
    serviceTokens.find("service1") should be equals (serviceToken1)
    serviceTokens.find("service2") should be equals (serviceToken2)
  }

  behavior of "Tokens"

  it should "be able to find the ServiceToken by service name" in {
    tokens.service("service1") should be equals (serviceToken1)
    tokens.service("service2") should be equals (serviceToken2)
    tokens.service("service3") should be equals (None)
  }

  it should "uphold encoding/decoding Tokens" in {
    def encodeDecode(toks: Tokens) : Tokens =
      TokensDecoder.decodeJson(TokensEncoder(toks)).fold[Tokens](e => emptyTokens, t => t)
    encodeDecode(tokens) should be (tokens)
  }

  behavior of "SessionDataTokenEncoder"

  it should "uphold encoding/decoding for SessionDataTokenEncoder" in {
    def validate(s: Tokens)(implicit ev: SessionDataEncoder[Tokens]): Try[Tokens] = {
      ev.decode(ev.encode(s))
    }
    // Validate
    validate(tokens) should be (Success(tokens))
  }

  it should "not decode invalid data" in {
    def invalid(input: Buf)(implicit ev: SessionDataEncoder[Tokens]): Try[Tokens] =
      ev.decode(input)

    invalid(SessionDataEncoder.encodeString.encode( """{ "a" : "b" }""")).failure.exception should be(a[SessionDataError])
  }
}
