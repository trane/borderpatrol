package com.lookout.borderpatrol.sessionx

import com.lookout.borderpatrol.sessionx.SecretStores.InMemorySecretStore

class SecretStoresSpec extends BorderPatrolSuite {
  import helpers.{secretStore => store, _}, secrets._

  behavior of "SecretStoreApi"

  it should "give the current and previous Secret" in {
    store.current shouldBe current
    store.previous shouldBe previous
  }

  it should "always give a non-expired current secret" in {
    // set the current to an expired secret
    val tempStore = InMemorySecretStore(Secrets(previous, previous))
    tempStore.current should not be previous
    tempStore.current.expired shouldBe false
  }

  it should "find secrets if they exist" in {
    store.find(_.id == previous.id).value shouldBe previous
    store.find(_.id == current.id).value shouldBe current
    store.find(_.id == invalid.id) shouldBe None
  }
}
