package com.lookout.borderpatrol.crypto

import java.security.Provider
import javax.crypto.{Cipher, SecretKey}
import javax.crypto.spec.IvParameterSpec
import com.lookout.borderpatrol.util.Combinators.tap

/**
 * Base components needed for a symmetric key
 * Utilized by [[com.lookout.borderpatrol.crypto.CryptKey CryptKey]]
 */
trait SymmetricKey {
  val keyAlgo: String
  val cipherAlgo: String
  val provider: Provider
  val key: SecretKey
  val iv: IvParameterSpec

  def cipher(mode: Int): Cipher =
    tap(Cipher.getInstance(cipherAlgo, provider.getName))(_.init(mode, key, iv))
}

