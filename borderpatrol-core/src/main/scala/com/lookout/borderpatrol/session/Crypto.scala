package com.lookout.borderpatrol.session

import java.security.{Provider, Security, Key, SecureRandom}
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import javax.crypto.{SecretKey, Cipher,  Mac}
import org.bouncycastle.jce.provider.BouncyCastleProvider

import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.util.{Duration, Time}

import scala.util.Random

trait RandomGenerator {
  val random: Random
}

object Generator extends RandomGenerator {
  val random = new Random(new SecureRandom)

  def apply(n: Int): Seq[Byte] =
    tap(Array.fill[Byte](n)(0))(random.nextBytes)
}

trait Expiry {
  val lifetime: Duration
  def currentExpiry: Time =
    Time.now + lifetime
}

trait Signer {
  val algo: String
  val key: Key
  lazy val hmac: Mac = tap(Mac.getInstance(algo))(mac => mac.init(key))

  def sign(bytes: Seq[Byte]): Seq[Byte] =
    hmac.doFinal(bytes.toArray)
}

trait SymmetricKey {
  val keyAlgo: String
  val cipherAlgo: String
  val provider: Provider
  val key: SecretKey
  val iv: IvParameterSpec

  def cipher(mode: Int): Cipher =
    tap(Cipher.getInstance(cipherAlgo, provider.getName))(_.init(mode, key, iv))

  def encrypt(bytes: Array[Byte]): Array[Byte]

  def decrypt(bytes: Array[Byte]): Array[Byte]
}

case class CryptKey(keyBytes: Array[Byte], ivBytes: Array[Byte], provider: Provider = new BouncyCastleProvider) extends SymmetricKey {
  Security.addProvider(provider)

  val keyAlgo: String = "PBKDF2WithHmacSHA1"
  val cipherAlgo: String = "AES/GCM/NoPadding"
  val key = new SecretKeySpec(keyBytes, keyAlgo)
  val iv = new IvParameterSpec(ivBytes)

  def encrypt(bytes: Array[Byte]): Array[Byte] =
    cipher(Cipher.ENCRYPT_MODE).doFinal(bytes.toArray)

  def decrypt(bytes: Array[Byte]): Array[Byte] =
    cipher(Cipher.DECRYPT_MODE).doFinal(bytes.toArray)
}

object CryptKey {
  def apply(id: SessionId, secret: Secret): CryptKey =
    CryptKey(id.entropy.toArray, secret.entropy.toArray)
}
