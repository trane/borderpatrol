package com.lookout.borderpatrol.crypto

import java.security.{Security, Provider}
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

import com.lookout.borderpatrol.sessionx.{SessionId, Session}
import com.twitter.io.Buf
import org.bouncycastle.jce.provider.BouncyCastleProvider

import scala.util.Try


case class CryptKey(keyBytes: Array[Byte],
                    ivBytes: Array[Byte],
                    provider: Provider = new BouncyCastleProvider)
    extends SymmetricKey {

  private[this] val providerPos = Security.addProvider(provider)

  val keyAlgo: String = "PBKDF2WithHmacSHA1"
  val cipherAlgo: String = "AES/GCM/NoPadding"
  val key = new SecretKeySpec(keyBytes, keyAlgo)
  val iv = new IvParameterSpec(ivBytes)

  def encrypt[A](a: A)(implicit f: A => Array[Byte]): Array[Byte] =
    cipher(Cipher.ENCRYPT_MODE).doFinal(f(a))

  def decrypt[A](bytes: Array[Byte])(implicit f: Array[Byte] => A): Try[A] =
    Try { f(cipher(Cipher.DECRYPT_MODE).doFinal(bytes)) }
}

/**
 * After some investigation into prototyping and fast ciphers, I have a few observations and an implementation
 * Observations:
 *   - It's clear that ChaCha and AES are winners in the speed category of ciphers
 *     (http://bench.cr.yp.to/results-stream.html)
 *   - Stream ciphers are probably the wrong use-case here because we most likely want authenticated encryption, which
 *     rules out ChaCha.
 *   - Most block ciphers require an IV (which must be unique, but can be predictable) *
 *   - AES-GCM is an authenticated encryption mechanism that could be very useful here *
 *
 * Implementation Details:
 *   I have chosen to make the IV be the SessionId.Entropy, since it is unique but predictable and public. The key
 *   then becomes the hashed SecretKey which is private.
 *
 *   CryptKey := AES-GCM( Key , Iv )
 *   Key := PBKDF2WithHmacSHA1( SecretKey )
 *   Iv := SessionId.Entropy
 *
 * This means that all sessions will be encrypted with the same secret, but they will have a unique IV based on the
 * entropy of the session id. Note: secrets are rotated daily.
 *
 * Alternatively, you could create a new SessionId every time Session data is stored
 */
object CryptKey {
  def apply(id: SessionId): CryptKey =
    CryptKey(id.entropy.toArray, id.secret.entropy.toArray)

  def apply(session: Session[_]): CryptKey =
    apply(session.id)

  implicit val bytesToBuf: Array[Byte] => Try[Buf] = a => Try { Buf.ByteArray.Owned(a) }
  implicit val bufToBytes: Buf => Array[Byte] = b => Buf.ByteArray.Owned.extract(b)
}

