package com.lookout.borderpatrol.test.sessionx

import java.net.URL

import com.lookout.borderpatrol.Binder.{BindRequest, MBinder}
import com.lookout.borderpatrol.auth.OAuth2.OAuth2CodeVerify
import com.lookout.borderpatrol._
import com.lookout.borderpatrol.auth.ServiceRequest
import com.lookout.borderpatrol.sessionx.SecretStores.InMemorySecretStore
import com.twitter.finagle.httpx.Method.{Put, Get}
import com.twitter.finagle.httpx.path.Path
import com.twitter.finagle.httpx.{RequestBuilder, Status, Response, Request}
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.util._
import com.twitter.finagle.httpx
import scala.util.{Success, Try}
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
   *
   */
  implicit val bpTestStatsReceiver = NullStatsReceiver

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

  //  urls
  val urls = Set(new URL("http://localhost:5678"))

  //  Managers
  val keymasterIdManager = Manager("keymaster", Path("/identityProvider"), urls)
  val keymasterAccessManager = Manager("keymaster", Path("/accessIssuer"), urls)
  val internalProtoManager = InternalAuthProtoManager(Path("/loginConfirm"), Path("/check"), urls)
  val checkpointLoginManager = LoginManager("checkpoint", keymasterIdManager, keymasterAccessManager,
    internalProtoManager)
  val oauth2CodeProtoManager = OAuth2CodeProtoManager(Path("/signin"),
    new URL("http://example.com/authorizeUrl"),
    new URL("http://localhost:4567/tokenUrl"),
    new URL("http://localhost:4567/certificateUrl"),
    "clientId", "clientSecret")
  val umbrellaLoginManager = LoginManager("ulm", keymasterIdManager, keymasterAccessManager,
    oauth2CodeProtoManager)
  val oauth2CodeBadProtoManager = OAuth2CodeProtoManager(Path("/signblew"),
    new URL("http://localhost:9999/authorizeUrl"),
    new URL("http://localhost:9999/tokenUrl"),
    new URL("http://localhost:9999/certificateUrl"),
    "clientId", "clientSecret")
  val rainyLoginManager = LoginManager("rlm", keymasterIdManager, keymasterAccessManager,
    oauth2CodeBadProtoManager)
  val loginManagers = Set(checkpointLoginManager, umbrellaLoginManager, rainyLoginManager)

  //  oAuth2 Code Verify object
  val oAuth2CodeVerify = new OAuth2CodeVerify

  // sids
  val one = ServiceIdentifier("one", urls, Path("/ent"), None)
  val oneTwo = ServiceIdentifier("oneTwo", urls, Path("/ent2"), None)
  val cust1 = CustomerIdentifier("enterprise", one, checkpointLoginManager)
  val two = ServiceIdentifier("two", urls, Path("/umb"), Some(Path("/broken/umb")))
  val cust2 = CustomerIdentifier("sky", two, umbrellaLoginManager)
  val three = ServiceIdentifier("three", urls, Path("/rain"), None)
  val cust3 = CustomerIdentifier("rainy", three, rainyLoginManager)
  val cids = Set(cust1, cust2, cust3)
  val sids = Set(one, oneTwo, two, three)
  val serviceMatcher = ServiceMatcher(cids, sids)
  val sessionStore = SessionStores.InMemoryStore

  // Test Services
  def mkTestService[A, B](f: (A) => Future[B]) : Service[A, B] = new Service[A, B] {
    def apply(request: A) = f(request)
  }

  // Request helper
  def req(subdomain: String, path: String, params: Tuple2[String, String]*): Request =
    RequestBuilder().url(s"http://${subdomain + "."}example.com${Request.queryString(path, params:_*)}").buildGet()

  // Binders
  case class TestManagerBinder() extends MBinder[Manager]
  def mkTestManagerBinder(f: (BindRequest[Manager]) => Future[Response]): TestManagerBinder = new TestManagerBinder {
    override def apply(request: BindRequest[Manager]) = f(request)
  }
  case class TestLoginManagerBinder() extends MBinder[LoginManager]
  def mkTestLoginManagerBinder(f: (BindRequest[LoginManager]) => Future[Response]): TestLoginManagerBinder =
    new TestLoginManagerBinder {
      override def apply(request: BindRequest[LoginManager]) = f(request)
    }
  case class TestServiceIdentifierBinder() extends MBinder[ServiceIdentifier]
  def mkTestSidBinder(f: (BindRequest[ServiceIdentifier]) => Future[Response]): TestServiceIdentifierBinder =
    new TestServiceIdentifierBinder {
      override def apply(request: BindRequest[ServiceIdentifier]) = f(request)
    }
}
