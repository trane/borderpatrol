package com.lookout.borderpatrol.session

import org.scalatest.{FlatSpec, Matchers}

class CryptoSpec extends FlatSpec with Matchers {

  behavior of "Generator"

  it should "create entropy the size of input" in {
    Generator(1) should have size 1
  }

  it should "create new values on apply" in {
    val g1 = Generator(2)
    val g2 = Generator(2)
    g1 should not equal g2
  }

  behavior of "CryptKey"
  import Session._
  val id = generator.next
  val bytes = Generator(16)

  it should "encrypt a sequence" in {
    val cryptKey = CryptKey.apply(id, secretStore.current)
    cryptKey.encrypt(bytes) should not equal (bytes)
    cryptKey.encrypt(bytes).size should be > bytes.size
  }

  it should "have the same output as input when decrypting what it encrypted" in {
    val cryptKey = CryptKey.apply(id, secretStore.current)
    cryptKey.decrypt(cryptKey.encrypt(bytes)) shouldEqual (bytes)
  }
}
