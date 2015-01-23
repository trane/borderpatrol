package com.lookout.borderpatrol.session

import java.util.concurrent.TimeUnit

import com.lookout.borderpatrol.session.id.{ExpiryComponent, Types}
import com.lookout.borderpatrol.session.secret.{SecretStoreComponent, SecretStoreApi}
import com.twitter.bijection._
import com.twitter.util.{Duration, Time}

import scala.util.{Failure, Success, Try}

import Types._

trait SessionId extends ExpiryComponent {
  val expires: Time
  val entropy: Entropy
  val secretId: SecretId
  val signature: Signature

  lazy val payload: Payload = timeBytes ++ entropy :+ secretId
  lazy val toSeq: Seq[Byte] = payload ++ signature
  lazy val toBytes: Array[Byte] = toSeq.toArray

  def timeBytes: TimeBytes = Injection.long2BigEndian(expires.inMilliseconds)
  def expired: Boolean = expires < Time.now || expires > currentExpiry
}

object SessionId {
  def apply(expires: Time, entropy: Entropy, secretId: Byte, signature: Signature): SessionId =
    Id(expires, entropy, secretId, signature)
  def unapply(s: SessionId): Option[(Time, Entropy, SecretId, Signature)] =
    Some((s.expires, s.entropy, s.secretId, s.signature))

  case class Id(expires: Time, entropy: Entropy, secretId: SecretId, signature: Signature) extends SessionId
}
