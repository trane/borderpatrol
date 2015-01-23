package com.lookout.borderpatrol.session

import com.lookout.borderpatrol.session.secret.InMemorySecretStore
import org.scalatest.{FlatSpec, Matchers}
import com.lookout.borderpatrol.session.id.{Generator => IdGenerator}

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
  implicit val secretStore = new InMemorySecretStore(Secrets.mockSecrets)
  val idGenerator = new IdGenerator

  val id = idGenerator.next
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
