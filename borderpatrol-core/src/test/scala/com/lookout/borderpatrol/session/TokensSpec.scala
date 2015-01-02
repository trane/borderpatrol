package com.lookout.borderpatrol.session

import org.scalatest.{Matchers, FlatSpec}

class TokensSpec extends FlatSpec with Matchers {

  val mockEmptyTokens = Tokens(EmptyToken, EmptyServiceTokens)
  val serviceName = "service1"
  val mockServiceTokens = ServiceTokens(Map(serviceName -> "token"))
  val testMasterToken = MasterToken("test")
  def mockServiceToken(name: String = serviceName, value: String = "value") =
    ServiceToken(name, value)
  def mockJsonResponse: String =
    """{"auth_service":"DEADBEEF","service_tokens":{"foo":"LIVEKALE","bar":"WOUNDEDCAKE","baz":"HOTCAFE"}}"""

  behavior of "ServiceTokens"

  it should "return itself when added to itself" in {
    EmptyServiceTokens ++ EmptyServiceTokens shouldBe EmptyServiceTokens
    mockServiceTokens ++ mockServiceTokens shouldBe mockServiceTokens
  }

  it should "return itself when EmptyServiceTokens are added to it" in {
    mockServiceTokens ++ EmptyServiceTokens shouldBe mockServiceTokens
  }

  it should "add a service token" in {
    (EmptyServiceTokens + mockServiceToken()).get(serviceName) getOrElse EmptyToken should not be EmptyToken
  }

  it should "replace an existing service token" in {
    val value = "newtokenvalue"
    val tok = (mockServiceTokens + mockServiceToken(serviceName, value)).get(serviceName).get
    tok.value shouldBe value
  }

  it should "return None when service token doesn't exist" in {
    EmptyServiceTokens.get("randomtoken") shouldBe None
    mockServiceTokens.get("randomtoken") shouldBe None
  }

  behavior of "Tokens"

  it should "add a master token" in {
    val t = mockEmptyTokens += testMasterToken
    t.master shouldBe testMasterToken
  }

  it should "not replace a master token with an EmptyToken" in {
    val t = (mockEmptyTokens += testMasterToken) += EmptyToken
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
    val merged = (tokens1 ++= tokens2.services)
    val mergedIdentity = merged ++= EmptyServiceTokens
    (merged.service("someservice") getOrElse EmptyToken) shouldBe st1
    (merged.service("otherservice") getOrElse EmptyToken) shouldBe st2
    mergedIdentity shouldBe merged
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

  it should "invert to the same JSON it converted from" in {
    TokenJson.TokensJson(TokenJson.TokensCodecJson.encode(TokenJson.TokensJson(mockJsonResponse).get).toString()) shouldEqual TokenJson.TokensJson(mockJsonResponse)
  }
}
