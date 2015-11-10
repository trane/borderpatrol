package com.lookout.borderpatrol.test.sessionx

import com.lookout.borderpatrol.sessionx.SecretStores.InMemorySecretStore
import com.twitter.bijection.Injection
import com.twitter.util.{Await, Time}

object helpers {
  import com.lookout.borderpatrol.sessionx._
  import com.lookout.borderpatrol.crypto.Generator.{EntropyGenerator => Entropy}
   /**
    * Common usage of secrets across tests
    */
   object secrets {
     val current = Secret(Injection.short2BigEndian(1), Secret.currentExpiry, Entropy(16))
     val previous = Secret(Injection.short2BigEndian(2), Time.fromMilliseconds(0), Entropy(16))
     val invalid = Secret(Injection.short2BigEndian(3), Time.now, Entropy(16)) // not in store
     val secrets = Secrets(current, previous)
   }
   implicit val secretStore = InMemorySecretStore(secrets.secrets)

   /**
    * Common usage of sessionid across tests
    */
   object sessionid {

     def untagged: SessionId =
       Await.result(SessionId.untagged)

     def authenticated: SessionId =
       Await.result(SessionId.authenticated)

     def expired: SessionId =
       SessionId(Time.fromMilliseconds(0), Entropy(16), secrets.current, Untagged)

     def invalid: SessionId =
       untagged.copy(entropy = Entropy(16))
   }

   object sessions {
     def create[A](a: A): Session[A] =
       Session(sessionid.untagged, a)
   }
 }
