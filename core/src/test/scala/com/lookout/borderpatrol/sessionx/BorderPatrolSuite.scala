package com.lookout.borderpatrol.sessionx

import com.lookout.borderpatrol.sessionx.SecretStores.InMemorySecretStore
import com.twitter.util.{Await, Time}
import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}

/**
 * A trait to provide consistency and reduce boilerplate for testing in Border Patrol
 */
trait BorderPatrolSuite extends FlatSpec with Matchers with TryValues with OptionValues

object helpers {
  import com.lookout.borderpatrol.sessionx.crypto.Generator.{EntropyGenerator => Entropy}
  /**
   * Common usage of secrets across tests
   */
  object secrets {
    val current = Secret(1.toByte, Secret.currentExpiry, Entropy(16))
    val previous = Secret(2.toByte, Time.fromMilliseconds(0), Entropy(16))
    val invalid = Secret(3.toByte, Time.now, Entropy(16)) // not in store
    val secrets = Secrets(current, previous)
  }
  implicit val secretStore = InMemorySecretStore(secrets.secrets)

  /**
   * Common usage of sessionid across tests
   */
  object sessionid {

    def next: SessionId =
      Await.result(SessionId.next)

    def expired: SessionId =
      SessionId(Time.fromMilliseconds(0), Entropy(16), secrets.current)

    def invalid: SessionId =
      next.copy(entropy = Entropy(16))
  }

  object sessions {
    def create[A](a: A): Session[A] =
      Session(sessionid.next, a)
  }
}

