package com.lookout.borderpatrol.session.id

import java.util.concurrent.TimeUnit

import com.lookout.borderpatrol.session.Expiry
import com.twitter.util.Duration

/**
 * Created by akuhnhausen on 1/22/15.
 */
trait ExpiryComponent extends Expiry {
  val lifetime = Duration(1, TimeUnit.DAYS)
}
