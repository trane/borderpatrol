/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Lookout, Inc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package com.lookout.borderpatrol.test.crypto

import java.security.Key
import javax.crypto.spec.SecretKeySpec

import com.lookout.borderpatrol.sessionx.{SignedId, Session}
import com.lookout.borderpatrol.crypto._
import com.lookout.borderpatrol.test._
import com.twitter.finagle.{http, memcached}
import com.twitter.io.Buf
import com.twitter.util.{Future, Await}

import scala.util.Try

class cryptoSpec extends BorderPatrolSuite {
  val entropy = Array[Byte](1, 2, 3)

  behavior of "Signer"

  val signer = new Signer {
    override val algo = "HmacSHA256"
    override val key: Key = new SecretKeySpec(entropy, algo)
  }

  it should "sign a sequence of bytes" in {
    signer.sign(entropy) should not be empty
  }

  it should "signing the same sequence of bytes regardless of datastructure should result in the same signature" in {
    val vecBytes = Vector[Byte](1, 2, 3)
    val arrBytes = Array[Byte](1, 2, 3)

    signer.sign(vecBytes) should be(signer.sign(arrBytes))
  }

  behavior of "Generator"

  it should "generate entropy of right length" in {
    val size = 16
    val ent = Generator.EntropyGenerator(size)
    val ent2 = Generator.BytesGenerator(size)
    ent should have length size
    ent2 should have length size
  }

  behavior of "CryptKey"

  val ckey = Generator.BytesGenerator(16).toArray
  val iv = Generator.BytesGenerator(16).toArray
  val cryptKey = CryptKey(ckey, iv)

  it should "encrypt and decrypt" in {
    val enc = cryptKey.encrypt(entropy)
    (enc sameElements entropy) shouldBe false
    (cryptKey.decrypt(enc).success.value sameElements entropy) shouldBe true
  }

  it should "encrypt anything that can be turned into an Array[Byte]" in {
    implicit def str2Bytes(s: String): Array[Byte] = s.getBytes()
    implicit def bytes2Str(b: Array[Byte]): String = new String(b)
    val str = "Hello"

    import com.lookout.borderpatrol.test.sessionx.helpers.secretStore
    import com.lookout.borderpatrol.crypto.Encryptable._

    def enc[A : Encryptable](session: Session[A]): Array[Byte] =
      implicitly[Encryptable[A]].encrypt(session)
    def dec[A : Decryptable](id: SignedId, bytes: Array[Byte]): Try[Session[A]] =
      implicitly[Decryptable[A]].decrypt(id, bytes)

    val id = SignedId.untagged.results
    dec[String](id, enc[String](Session(id, "hello"))).success.value should be(Session(id, "hello"))
  }

  it should "automatically convert to/from com.twitter.io.Buf" in {
    import CryptKey.{bufToBytes, bytesToBuf}
    val buf = Buf.ByteArray.Owned(Generator.BytesGenerator(16).toArray)
    val key = CryptKey(ckey, iv)
    key.decrypt[Buf](key.encrypt(buf)).success.value should be(buf)
  }

  behavior of "EncryptedSessionStore"
  import com.lookout.borderpatrol.test.sessionx.helpers._

  val store = EncryptedSessionStore.MemcachedStore(new memcached.MockClient())
  val strSession = Session("hello").results
  val intSession = Session(1).results
  val reqSession = Session(http.Request("/api")).results

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
}
