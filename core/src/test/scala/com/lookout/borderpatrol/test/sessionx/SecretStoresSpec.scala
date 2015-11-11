package com.lookout.borderpatrol.sessionx

import com.lookout.borderpatrol.sessionx.SecretStores._
import com.lookout.borderpatrol.test._
import com.lookout.borderpatrol.test.sessionx.helpers

class SecretStoresSpec extends BorderPatrolSuite {
  import sessionx.helpers.{secretStore => store, _}, secrets._

  behavior of "SecretStoreApi"

  it should "give the current and previous Secret" in {
    store.current shouldBe current
    store.previous shouldBe previous
  }

  it should "always give a non-expired current secret" in {
    // set the current to an expired secret
    val tempStore = SecretStores.InMemorySecretStore(Secrets(previous, previous))
    tempStore.current should not be previous
    tempStore.current.expired shouldBe false
  }

  it should "find secrets if they exist" in {
    store.find(_.id == previous.id).value shouldBe previous
    store.find(_.id == current.id).value shouldBe current
    store.find(_.id == invalid.id) shouldBe None
  }

  behavior of "ConsulSecretStore"

  val secretsJsonString = SecretsEncoder.EncodeJson.encode(helpers.secrets.secrets).nospaces
  it should "always return a secret" in {
    val cc = new ConsulConnection(mockService(helpers.secrets.secrets),"localhost","8500")
    val s = new ConsulSecretStore(cc,10,Secrets(previous, previous))//initialize the cache with expired secrets
    s.current should not be null
    s.previous should not be null
  }

  it should "always return a secret when there is a problem with secrets" in {
    val consulConnection = helpers.MockConsulClient
    consulConnection.set(ConsulSecretsKey, "{'invalid':'json'}")
    val c = new ConsulSecretStore(consulConnection, 10, helpers.secrets.secrets)
    c.current shouldBe current
    c.previous shouldBe previous
  }

  it should "Rotate expired secrets out of the cache" in {
    val cc = new ConsulConnection(mockService(helpers.secrets.secrets),"localhost","8500")
    val s = new ConsulSecretStore(cc,10,Secrets(previous, previous))//initialize the cache with expired secrets
    Thread.sleep(1000) //allows the polling function to start appending
    s.current shouldBe current
    s.previous shouldBe previous
  }

  it should "rotate and return the current secret after finding a new current secret" in {
    val cc = new ConsulConnection(mockService(helpers.secrets.secrets),"localhost","8500")
    val s = new ConsulSecretStore(cc, 10, Secrets(previous, previous))
    Thread.sleep(1000) //allows the polling function to start appending
    s.find(_.id == current.id)
    s.current shouldBe current
  }
}
