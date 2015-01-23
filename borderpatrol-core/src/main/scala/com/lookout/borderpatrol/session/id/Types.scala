package com.lookout.borderpatrol.session.id

/**
 * Created by akuhnhausen on 1/22/15.
 */
object Types {
  type Seconds = Long
  type Size = Int
  type TimeBytes = IndexedSeq[Byte]
  type Entropy = IndexedSeq[Byte]
  type SecretId = Byte
  type Signature = IndexedSeq[Byte]
  type Payload = IndexedSeq[Byte]
}
