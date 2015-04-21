package com.lookout.borderpatrol.session.id

object Types {
  type Seconds = Long
  type Size = Int
  type TimeBytes = IndexedSeq[Byte]
  type Entropy = IndexedSeq[Byte]
  type SecretId = Byte
  type Signature = IndexedSeq[Byte]
  type Payload = IndexedSeq[Byte]
}
