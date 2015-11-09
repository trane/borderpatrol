package com.lookout.borderpatrol.test.sessionx

import com.lookout.borderpatrol.sessionx.SecretStores.InMemorySecretStore
import com.twitter.util.{Future, Await, Time}
import com.twitter.finagle.httpx

import scala.util.{Success, Try}

object helpers {
  import com.lookout.borderpatrol.sessionx._
  import com.lookout.borderpatrol.crypto.Generator.{EntropyGenerator => Entropy}
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

  object MockConsulClient extends ConsulConnection(null,"test","8500") {
    val mockRemote = collection.mutable.HashMap.empty[String, String]

    override def value(key: String): Future[Try[String]] =
      Future.value(Success(mockRemote(key)))

    override def set(k: String, v: String): Future[httpx.Response] = {
      mockRemote += (k -> v)
      Future.value(httpx.Response(httpx.Status.Ok))
    }

  }
 }
