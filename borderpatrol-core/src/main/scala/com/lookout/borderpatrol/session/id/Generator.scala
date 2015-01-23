package com.lookout.borderpatrol.session.id

import Types._
import com.lookout.borderpatrol.session.secret.SecretStoreApi
import com.lookout.borderpatrol.session.{SessionId, Generator, Secret}
import com.twitter.util.Time

class Generator extends ExpiryComponent {
  import com.lookout.borderpatrol.session.Constants.SessionId.entropySize

  def next(implicit store: SecretStoreApi): SessionId = {
    val entropy = Generator(entropySize).toVector
    val secret = store.current
    val temp = Id(currentExpiry, entropy)(secret)
    SessionId(temp.expires, temp.entropy, temp.secretId, temp.signature)
  }

  case class Id(expires: Time, entropy: Entropy)(secret: Secret) extends SessionId {
    val secretId = secret.id
    val signature = secret.sign(payload).toVector
  }
}
