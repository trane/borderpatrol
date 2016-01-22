package com.lookout.borderpatrol.sessionx

import com.lookout.borderpatrol.test._
import com.twitter.util.{Future, Await}
import com.twitter.finagle.http
import com.twitter.finagle.memcached

class SessionStoreSpec extends BorderPatrolSuite {
  import sessionx.helpers._

  behavior of "SessionStore"

  val sessionStore = SessionStores.InMemoryStore
  val memcachedSessionStore = SessionStores.MemcachedStore(new memcached.MockClient())
  val intSession = sessions.create(1)
  val strSession = sessions.create("hello")
  val reqSession = sessions.create(http.Request("localhost:8080/api/hello"))

  val stores: List[SessionStore] = List(sessionStore, memcachedSessionStore)

  stores.map { store =>
    /* setup */
    Await.all(
      store.update[Int](intSession),
      store.update[String](strSession),
      store.update[http.Request](reqSession)
    )

    it should s"fetch sessions that are stored in $store" in {
      store.get[String](strSession.id).results.value.data shouldEqual strSession.data
      store.get[Int](intSession.id).results.value.data shouldBe intSession.data
    }

    it should s"return a None when not present in $store" in {
      store.get[Int](sessionid.untagged).results shouldBe None
    }

    it should s"store request sessions $store" in {
      store.get[http.Request](reqSession.id).results.get.data.uri shouldEqual reqSession.data.uri
    }

    it should s"return a Future exception when decoding to wrong type in $store" in {
      // try to make an Session[Int] => Session[http.Request]
      store.get[http.Request](intSession.id).isThrowable should be(true)

      /* TODO: Disallow this: Int -> Buf -> String
      isThrow(store.get[Int](strSession.id)) should be(false)
      */
    }

    it should s"delete stored values in $store" in {
      store.update(intSession)
      store.get[Int](intSession.id).results shouldBe Some(intSession)
      store.delete(intSession.id)
      store.get[Int](intSession.id).results shouldBe None
    }
  }

}
