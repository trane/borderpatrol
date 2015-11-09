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
    val consulConnection = helpers.MockConsulClient
    consulConnection.set(ConsulSecretsKey, secretsJsonString)
    val c = new ConsulSecretStore(consulConnection, 10, helpers.secrets.secrets)
    c.current should not be null
    c.previous should not be null
  }

  it should "always return a secret when there is a problem with secrets" in {
    val consulConnection = helpers.MockConsulClient
    consulConnection.set(ConsulSecretsKey, "{'invalid':'json'}")
    val c = new ConsulSecretStore(consulConnection, 10, helpers.secrets.secrets)
    c.current shouldBe current
    c.previous shouldBe previous
  }

  it should "rotate and return the current secret after finding a new current secret" in {
    val newConsulConnection = helpers.MockConsulClient
    newConsulConnection.set(ConsulSecretsKey, secretsJsonString)
    val c = new ConsulSecretStore(newConsulConnection, 10, Secrets(previous, previous))
    Thread.sleep(1000) //allows the polling function to start appending
    c.find(_.id == current.id)
    c.current shouldBe current
  }

  it should "rotate when the secret is expired" in {
    val newConsulConnection = helpers.MockConsulClient
    newConsulConnection.set(ConsulSecretsKey, secretsJsonString)
    val c = new ConsulSecretStore(newConsulConnection, 10, Secrets(previous, previous))
    Thread.sleep(1000) //allows the polling function to start appending
    c.current shouldBe current
  }
}
