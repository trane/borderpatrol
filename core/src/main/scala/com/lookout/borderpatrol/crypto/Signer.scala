package com.lookout.borderpatrol.crypto

import java.security.Key
import javax.crypto.Mac
import com.lookout.borderpatrol.util.Combinators.tap

/**
 * Mixin for being able to sign bytes
 * It implements HMAC, recommended to use a decent hashing algorithm like
 * SHA256
 */
trait Signer {
  val algo: String
  val key: Key
  lazy val hmac: Mac = tap(Mac.getInstance(algo))(mac => mac.init(key))

  def sign(bytes: IndexedSeq[Byte]): Signature =
    hmac.doFinal(bytes.toArray).toIndexedSeq
}

