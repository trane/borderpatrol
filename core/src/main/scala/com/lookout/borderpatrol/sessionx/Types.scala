package com.lookout.borderpatrol.sessionx

trait Types {
  type Encrypted = Array[Byte]
  type Seconds = Long
  type Size = Int
  type TimeBytes = IndexedSeq[Byte]
  type Entropy = IndexedSeq[Byte]
  type SecretId = Byte
  type TagId = Byte
  type Payload = IndexedSeq[Byte]
  type Signature = IndexedSeq[Byte]

}
