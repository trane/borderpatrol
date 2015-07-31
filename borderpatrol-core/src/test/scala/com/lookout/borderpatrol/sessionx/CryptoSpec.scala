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

package com.lookout.borderpatrol.sessionx

import java.security.Key

import javax.crypto.spec.SecretKeySpec
import crypto._

import org.scalatest.{FlatSpec, Matchers}

class cryptoSpec extends FlatSpec with Matchers {
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
    ent should have length size
  }

  behavior of "CryptKey"

  val ckey = Generator.BytesGenerator(16).toArray
  val iv = Generator.BytesGenerator(16).toArray
  val cryptKey = crypto.CryptKey(ckey, iv)

  it should "encrypt and decrypt" in {
    val enc = cryptKey.encrypt(entropy)
    (enc sameElements entropy) shouldBe false
    (cryptKey.decrypt(enc) sameElements entropy) shouldBe true
  }

  it should "encrypt anything that can be turned into an Array[Byte]" in {
    implicit def str2Bytes(s: String): Array[Byte] = s.getBytes()
    implicit def bytes2Str(b: Array[Byte]): String = new String(b)
    val str = "Hello"

    val enc = cryptKey.encrypt[String](str)
    val dec = cryptKey.decryptAs[String](enc)
    enc should not be str
    dec shouldBe str
  }
}
