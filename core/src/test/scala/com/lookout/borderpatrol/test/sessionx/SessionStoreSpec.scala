package com.lookout.borderpatrol.sessionx

import com.lookout.borderpatrol.test._
import com.twitter.util.{Future, Await}
import com.twitter.finagle.httpx
import com.twitter.finagle.memcachedx

class SessionStoreSpec extends BorderPatrolSuite {
  import sessionx.helpers._

  behavior of "SessionStore"

  val sessionStore = SessionStore.InMemoryStore
  val memcachedSessionStore = SessionStore.MemcachedStore(new memcachedx.MockClient())
  val intSession = sessions.create(1)
  val strSession = sessions.create("hello")
  val reqSession = sessions.create(httpx.Request("localhost:8080/api/hello"))

  val stores: List[SessionStore] = List(sessionStore, memcachedSessionStore)

  def result[A](f: Future[A]): A =
    Await.result(f)

  def isThrow[A](f: Future[A]): Boolean =
    Await.ready(f).poll.get.isThrow

  stores.map { store =>
    /* setup */
    Await.all(
      store.update[Int](intSession),
      store.update[String](strSession),
      store.update[httpx.Request](reqSession)
    )

    it should s"fetch sessions that are stored in $store" in {
      result(store.get[String](strSession.id)).value.data shouldEqual strSession.data
      result(store.get[Int](intSession.id)).value.data shouldBe intSession.data
    }

    it should s"return a None when not present in $store" in {
      result(store.get[Int](sessionid.next)) shouldBe None
    }

    it should s"store request sessions $store" in {
      result(store.get[httpx.Request](reqSession.id)).get.data.uri shouldEqual reqSession.data.uri
    }

    it should s"return a Future exception when decoding to wrong type in $store" in {
      // try to make an Session[Int] => Session[httpx.Request]
      isThrow(store.get[httpx.Request](intSession.id)) should be(true)

      /* TODO: Disallow this: Int -> Buf -> String
      isThrow(store.get[Int](strSession.id)) should be(false)
      */
    }
  }

}
