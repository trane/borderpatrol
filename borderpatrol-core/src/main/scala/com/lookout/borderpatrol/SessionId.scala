package com.lookout.borderpatrol

import java.util.concurrent.TimeUnit
import javax.crypto.spec.SecretKeySpec

import com.twitter.bijection.Injection
import com.twitter.util.{Duration, Time}


sealed trait SecureSessionId {
  type Seconds = Long
  type Size = Int
  type TimeBytes = Seconds => Array[Byte]
  type Entropy = Size => Array[Byte]
  type Signature = (Signer, Array[Byte]) => Array[Byte]

  /**
  val ts: TimeBytes
  val data: Entropy
  val payload: Payload
  val sig: Signature
    */
}


object SessionIdGenerator extends SecureSessionId with Expiry {
  val size: Size = 16
  val lifetime = Duration(1, TimeUnit.DAYS)

  def timeBytes: TimeBytes = Injection.long2BigEndian(_)
  def entropy: Entropy = Generator(_)
  def sig: Signature = (s, bytes) => s.sign(bytes)

  def apply: SecureSessionId = {
    val exp = currentExpiry.inLongSeconds
    val time = timeBytes(exp)
    val entropy = entropy(size)
    val s = sig(SecretStore.current, time ++ entropy)
    SessionId(time, entropy, s)
  }

  case class SessionId(expires: Array[Byte], entropy: Array[Byte], sig: Array[Byte]) extends SecureSessionId {

  }
}



/*
case class SessionId(ts: Long = SessionCrypto.currentExpiry,
                     data: Array[Byte] = SessionCrypto.data(SessionCrypto.dataLength))(secret: Secret) {

  val sigPayload = Injection.long2BigEndian(ts) ++ data
  val sig = SessionCrypto.sign(secret.key, sigPayload)
  lazy val repr = SessionCrypto.encode(this)

  def valid(otherSig: Array[Byte]): Boolean =
    (sig == otherSig) &&
      (ts > Time.now.inLongSeconds && ts < SessionCrypto.currentExpiry) &&
      (data.size == SessionCrypto.dataLength)

  override def toString: String = repr
}
*/
