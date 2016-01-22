package com.lookout.borderpatrol.sessionx

import argonaut.Json
import com.lookout.borderpatrol.test._
import com.twitter.io.Buf
import com.twitter.finagle.http

import scala.util.{Try, Success}


class EncoderSpec extends BorderPatrolSuite {
  import sessionx.helpers._

  behavior of "SecretEncoder"

  it should "be creatable" in {
    val secret = Secret()
    val secretEncoder = SecretEncoder[String](
      secret => "hello",
      string => Success(secret)
    )
    secretEncoder.encode(secret) should be("hello")
    secretEncoder.decode("hello").success.value should be(secret)
  }

  it should "uphold encoding/decoding identity" in {
    def identity(s: Secret)(implicit ev: SecretEncoder[Json]): Secret =
      ev.decode(ev.encode(s)) getOrElse Secret()

    identity(secrets.current) should be(secrets.current)
  }

  it should "not decode invalid data" in {
    def invalid[A](input: A)(implicit ev: SecretEncoder[A]): Try[Secret] =
      ev.decode(input)

    invalid[Json](Json.jNumber(1)).failure.exception should be(a[SecretDecodeError])
  }

  behavior of "SignedIdEncoder"

  it should "uphold encoding/decoding identity" in {
    def identity[A](id: SignedId)(implicit ev: SignedIdEncoder[A]): SignedId =
      ev.decode(ev.encode(id)) getOrElse sessionid.untagged

    val id = sessionid.untagged
    identity[String](id) should be(id)
  }

  it should "not decode invalid data" in {
    def invalid[A](input: A)(implicit ev: SignedIdEncoder[A]): Try[SignedId] =
      ev.decode(input)

    invalid[String]("forged secret session id").failure.exception should be(a[SignedIdError])
  }

  behavior of "SessionDataEncoder"

  it should "uphold encoding/decoding identity" in {
    def identity[A](a: A)(implicit ev: SessionDataEncoder[A]): A =
      ev.decode(ev.encode(a)).get

    val data = "hello"
    identity(data) should be(data)
  }

  it should "not decode invalid data" in {
    def invalid[A](input: Buf)(implicit ev: SessionDataEncoder[A]): Try[A] =
      ev.decode(input)

    invalid[http.Request](Buf.U32BE(1)).failure.exception should be(a[SessionDataError])
  }

  behavior of "SecretsEncoder"

  it should "Encode then decode Secrets and they should be the same" in {
    val a1 = Secret()
    val b1 = Secret()
    val c = SecretsEncoder.EncodeJson.encode(Secrets(a1,b1))
    SecretsEncoder.EncodeJson.decode(c).success.value.current should be(a1)
    SecretsEncoder.EncodeJson.decode(c).success.value.previous should be(b1)
  }

}
