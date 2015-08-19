package com.lookout.borderpatrol.sessionx

object crypto {
  import java.security.{Key, Provider, SecureRandom, Security}
  import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
  import javax.crypto.{Cipher, Mac, SecretKey}

  import com.lookout.borderpatrol.util.Combinators.tap
  import org.bouncycastle.jce.provider.BouncyCastleProvider

  import scala.util.Random

  trait Generator[+A] {
    val random: Random = new Random(new SecureRandom)

    def apply(n: Int): A
  }

  object Generator {

    implicit object BytesGenerator extends Generator[Seq[Byte]] {
      def apply(n: Int): Seq[Byte] =
        tap(Array.fill[Byte](n)(0))(random.nextBytes)
    }

    implicit object IndexedBytesGenerator extends Generator[IndexedSeq[Byte]] {
      def apply(n: Int): IndexedSeq[Byte] =
        tap(Array.fill[Byte](n)(0))(random.nextBytes).toIndexedSeq
    }

    implicit object EntropyGenerator extends Generator[Entropy] {
      def apply(n: Int): Entropy =
        tap(Array.fill[Byte](n)(0))(random.nextBytes).toIndexedSeq
    }

  }

  trait Signer {
    val algo: String
    val key: Key
    lazy val hmac: Mac = tap(Mac.getInstance(algo))(mac => mac.init(key))

    def sign(bytes: Seq[Byte]): Signature =
      hmac.doFinal(bytes.toArray).toIndexedSeq

    def sign(bytes: IndexedSeq[Byte]): Signature =
      hmac.doFinal(bytes.toArray).toIndexedSeq
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
      encrypt(f(a))

    def decryptAs[A](bytes: Array[Byte])(implicit f: Array[Byte] => A): A =
      f(decrypt(bytes))

    def encrypt(bytes: Array[Byte]): Array[Byte] =
      cipher(Cipher.ENCRYPT_MODE).doFinal(bytes)

    def decrypt(bytes: Array[Byte]): Array[Byte] =
      cipher(Cipher.DECRYPT_MODE).doFinal(bytes)
  }

  object CryptKey {

    implicit def id2Key(id: SessionId): Array[Byte] =
      id.entropy.toArray
    implicit def sec2Iv(secret: Secret): Array[Byte] =
      secret.entropy.toArray

    def apply(id: SessionId, secret: Secret): CryptKey =
      new CryptKey(id2Key(id), sec2Iv(secret))

    def apply(id: SessionId): CryptKey =
      apply(id, id.secret)

    def apply(s: Session[_]): CryptKey =
      apply(s.id)
  }

}
