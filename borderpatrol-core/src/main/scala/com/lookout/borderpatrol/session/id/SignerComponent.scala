package com.lookout.borderpatrol.session.id

import com.lookout.borderpatrol.session.secret.{SecretStoreApi, SecretStoreComponent}

/**
 * Created by akuhnhausen on 1/22/15.
 */
trait SignerComponent {
  this: SecretStoreComponent with ExpiryComponent =>
    implicit val secretStore: SecretStoreApi
}
