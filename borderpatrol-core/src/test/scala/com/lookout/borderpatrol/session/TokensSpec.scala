package com.lookout.borderpatrol.session

import com.lookout.borderpatrol.session.TokenJson.TokensJson
import org.scalatest.{Matchers, FlatSpec}

class TokensSpec extends FlatSpec with Matchers {

  val mockEmptyTokens = Tokens(EmptyToken, EmptyServiceTokens)
  val testMasterToken = MasterToken("test")
  val serviceName = "service1"
  def mockServiceToken(name: String = serviceName, value: String = "value") =
    ServiceToken(name, value)
  def mockJsonResponse: String =
    """
       {
            "auth_service": "DEADBEEF",
            "service_tokens": {
                "foo": "LIVEKALE",
                "bar": "WOUNDEDCAKE",
                "baz": "HOTCAFE"
            }
        }
    """

  behavior of "Tokens"

  it should "add a master token" in {
    val t = mockEmptyTokens += testMasterToken
    t.master shouldBe testMasterToken
  }

  it should "add a service token" in {
    val tok = mockServiceToken()
    val t = mockEmptyTokens += tok
    (t.service(serviceName) getOrElse EmptyToken) shouldBe tok
  }

  it should "replace an existing service token" in {
    val tok = mockServiceToken(serviceName, "value")
    val st = mockServiceToken(serviceName, "newvalue")
    val t = (mockEmptyTokens += tok) += st
    (t.service(serviceName) getOrElse EmptyToken) shouldBe st
  }

  it should "converge service tokens" in {
    val st1 = mockServiceToken("someservice")
    val st2 = mockServiceToken("otherservice")
    val tokens1 = mockEmptyTokens += st1
    val tokens2 = mockEmptyTokens += st2
    val merged = (tokens1 ++= tokens2)
    (merged.service("someservice") getOrElse EmptyToken) shouldBe st1
    (merged.service("otherservice") getOrElse EmptyToken) shouldBe st2
  }

  it should "replace all tokens from left with right" in {
    val st1 = mockServiceToken("someservice", "value")
    val st2 = mockServiceToken("someservice", "newvalue")
    val mt1 = MasterToken("value")
    val mt2 = MasterToken("newvalue")
    val tokens1 = mockEmptyTokens += mt1 += st1
    val tokens2 = mockEmptyTokens += mt2 += st2
    val merged = (tokens1 ++= tokens2)
    (merged.service("someservice") getOrElse EmptyToken) shouldBe st2
    merged.master shouldBe mt2
  }

  behavior of "ServiceTokens"

  it should "allow a service token to be added" in {
    ((EmptyServiceTokens + mockServiceToken()).get(serviceName) getOrElse EmptyToken) should not be EmptyToken
  }

  it should "merge service tokens" in {
    val t1 = EmptyServiceTokens + mockServiceToken("service0")
    val t2 = EmptyServiceTokens + mockServiceToken("service1")
    val merged = t1 ++ t2
    (merged.get("service0") getOrElse EmptyToken) should not be EmptyToken
    (merged.get("service1") getOrElse EmptyToken) should not be EmptyToken
  }

  behavior of "TokensJson"

  it should "hydrate Tokens" in {
    (TokenJson.TokensJson(mockJsonResponse) getOrElse mockEmptyTokens) should not be mockEmptyTokens
  }

  it should "hydrate MasterToken" in {
    (TokenJson.MasterTokenJson(mockJsonResponse) getOrElse EmptyToken) should not be EmptyToken
  }

  it should "hydrate ServiceTokens" in {
    (TokenJson.ServiceTokensJson(mockJsonResponse) getOrElse EmptyServiceTokens) should not be EmptyServiceTokens
  }
}
