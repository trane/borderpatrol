Border Patrol introduces types and functions that enable identifying, fetching, and storing web session data. This
is accomplished by a set of types that will be used by consumers of this library: `Session`, `Store`, and `Secret`.

A `com.lookout.borderpatrol.sessionx.Secret Secret` is a cryptographically verifiable signing key used to sign a
`com.lookout.borderpatrol.sessionx.SessionId SessionId`. Creating a `Secret` is simple. It defaults to expire at
`com.lookout.borderpatrol.sessionx.Secret.lifetime Secret.lifetime`


```tut:silent
import argonaut._, Argonaut._
import com.twitter.util.{Time, Await}
import com.twitter.finagle.httpx.Cookie
import com.lookout.borderpatrol.sessionx.{Secret, Secrets, Session, SessionId, SessionStore, SessionDataEncoder}
import com.lookout.borderpatrol.sessionx.crypto.Generator.EntropyGenerator
import com.lookout.borderpatrol.sessionx.SecretStores.InMemorySecretStore
import com.lookout.borderpatrol.sessionx.SecretStores
import com.twitter.finagle.httpx
import com.twitter.io.Buf
```

```tut
  val secret = Secret() // default secret expiry
  val expiringSecret = Secret(Time.now)

  val randomBytes = EntropyGenerator(16) // 16 bytes of randomness
  val randomId = EntropyGenerator(1).head // 1 byte of randomness for an id
  val expiry = Time.fromSeconds(0) // very expired
  val constructedSecret = Secret(randomId, expiry, randomBytes)
  println(s"secret has expired: ${constructedSecret.expired == true}")

  val signedMsg = secret.sign("message to by signed".getBytes)
```

A `com.lookout.borderpatrol.sessionx.SessionId SessionId` is a cryptographically signed identifier for a
`com.lookout.borderpatrol.sessionx.Session Session`, it consists of entropy, expiry, secret,and signature of those
items. This is meant to be used as the `com.twitter.finagle.httpx.Cookie` value, so we provide serializing to
`String`.

```tut
  implicit val secretStore = SecretStores.InMemorySecretStore(Secrets(Secret(), Secret()))
  val id: SessionId = Await.result(SessionId.next)
  val cookieValue: String = id.asBase64
  SessionId.from[String](cookieValue) == id
```

A `com.lookout.borderpatrol.sessionx.Session Session` is product type of a cryptographically verifiable
identifier `com.lookout.borderpatrol.sessionx.SessionId SessionId` and an arbitrary data type
A`. The only requirement for a `com.lookout.borderpatrol.sessionx.SessionStore SessionStore`[B,M] to store/fetch
a `Session[A]` is that there be some implicit injective views from `A => B` and `B => Try[A]`.

We have provided default encodings for: `httpx.Request => Buf`, `String => Buf` and their injective views.

```tut
   implicit val encodeRequest: SessionDataEncoder[httpx.Request] = SessionDataEncoder(
     data => Buf.ByteArray.Owned(data.encodeBytes()),
     buf => httpx.Request.decodeBytes(Buf.ByteArray.Owned.extract(buf))
   )

 // set up secret/session stores
 implicit val secretStore = SecretStores.InMemorySecretStore(Secrets(Secret(), Secret()))
 val sessionStore = SessionStore.InMemoryStore

 // create a Session[httpx.Request]
 val newSessionFuture = Session(httpx.Request("http://localhost/api/stuff")) // entropy is blocking on the JVM
 val newSession = Await.result(newSessionFuture)

 // see if the session expired (checks the `SessionId.expires`)
 println(s"Session has expired? ${newSession.expired}")

 // store the session and then fetch it
 sessionStore.update(newSession).onFailure(println)
 sessionStore.get(newSession.id).onSuccess(s => s match {
   case Some(s) => println(s"Same session?: ${newSession == s}")
   case None => println("hrm, where did the session go?")
 })
```

Let's say you have a `com.lookout.borderpatrol.sessionx.Session.data Session.data` type that doesn't have the
injective that you need, that's OK!
Assuming you are storing it in memcached, which requires a type of `com.twitter.io.Buf Buf` for the value:

```tut
  trait Foo {
    val value: Int
  }

  implicit val enc = SessionDataEncoder[Foo](
    foo => Buf.U32BE(foo.value),
    buf => new Foo { override val value = Buf.U32BE.unapply(buf).get._1 }
  )

  val foo1 = new Foo { override val value = 1 }
  val fooSession = Session(foo1)
  sessionStore.update(Await.result(fooSession))
```
