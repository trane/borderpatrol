package com.lookout.borderpatrol.session

import java.security.{Security, Key, SecureRandom}
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
  val keyAlgo: String = "PBKDF2WithHmacSHA1"
  val cipherAlgo: String = "AES/GCM/NoPadding"
  val key: SecretKey
  val iv: IvParameterSpec

  def cipher(mode: Int): Cipher =
    tap(Cipher.getInstance(cipherAlgo, "BC"))(c => c.init(mode, key, iv))

  def encrypt(bytes: Seq[Byte]): Seq[Byte]

  def decrypt(bytes: Seq[Byte]): Seq[Byte]
}

case class CryptKey(id: SessionId, secret: Secret) extends SymmetricKey {
  Security.addProvider(new BouncyCastleProvider())

  val key = new SecretKeySpec(secret.entropy.toArray, keyAlgo)
  val iv = new IvParameterSpec(id.entropy.toArray)

  def encrypt(bytes: Seq[Byte]): Seq[Byte] =
    cipher(Cipher.ENCRYPT_MODE).doFinal(bytes.toArray)

  def decrypt(bytes: Seq[Byte]): Seq[Byte] =
    cipher(Cipher.DECRYPT_MODE).doFinal(bytes.toArray)
}
