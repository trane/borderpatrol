package com.lookout.borderpatrol.test.sessionx

import com.lookout.borderpatrol.sessionx.SecretStores.InMemorySecretStore
import com.twitter.finagle.httpx.Method.{Put, Get}
import com.twitter.finagle.httpx.{Response, Request}
import com.twitter.util._
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
  import com.twitter.finagle.Service
  def mockConsulResponse(sec: Secrets): String = {
    val mockJson = SecretsEncoder.EncodeJson.encode(sec).nospaces
    val encodedJson = Base64StringEncoder.encode(mockJson.getBytes)
      s"""[{"CreateIndex":8,
    "ModifyIndex":104,"LockIndex":0,"Key":"secretStore/secrets","Flags":0,"Value":"$encodedJson"}]"""
  }

  def mockService(secrets: Secrets): Service[httpx.Request,httpx.Response] = {
    new Service[httpx.Request,httpx.Response] {
      val res = Response(httpx.Status.Ok)
      def apply(req: Request): Future[Response] = {
        (req.method,req.path) match {
          case (Get,_) => {res.setContentString(mockConsulResponse(secrets));Future.value(res)}
          case (Put,_) => {val n = new Promise[Response];n.setValue(res); n}
        }
      }
    }
  }
}
