package com.lookout.borderpatrol.sessionx

import argonaut.Json
import com.twitter.io.Buf
import com.twitter.finagle.httpx

import scala.util.{Try, Success}


class EncoderSpec extends BorderPatrolSuite {
  import helpers._

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

  behavior of "SessionIdEncoder"

  it should "uphold encoding/decoding identity" in {
    def identity[A](id: SessionId)(implicit ev: SessionIdEncoder[A]): SessionId =
      ev.decode(ev.encode(id)) getOrElse sessionid.next

    val id = sessionid.next
    identity[String](id) should be(id)
  }

  it should "not decode invalid data" in {
    def invalid[A](input: A)(implicit ev: SessionIdEncoder[A]): Try[SessionId] =
      ev.decode(input)

    invalid[String]("forged secret session id").failure.exception should be(a[SessionIdError])
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

    invalid[httpx.Request](Buf.U32BE(1)).failure.exception should be(a[SessionDataError])
  }
}
