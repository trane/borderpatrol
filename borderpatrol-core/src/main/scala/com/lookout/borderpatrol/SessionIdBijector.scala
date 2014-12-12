package com.lookout.borderpatrol

import scala.util.{Failure, Success, Try}

trait SessionIdBijector {
  type Timestamp = Array[Byte]
  type Data = Array[Byte]
  type Signature = Array[Byte]
  type BytesTuple = (Timestamp, Data, Signature)

  def parseBytes(bytes: Array[Byte]): Try[BytesTuple] = bytes match {
    case a if a.size == SessionCrypto.sessionByteLength => {
      val (ts, rest) = a.splitAt(SessionCrypto.tsLength)
      val (data, sig) = rest.splitAt(SessionCrypto.dataLength)
      Success(ts, data, sig)
    }
    case _ => Failure(new Exception("Invalid session string"))
  }

  def sessionIdWithSecrets(tuple: BytesTuple): Try[SessionId] = tuple match {
    case t if SessionCrypto.validateWithSecret(t)(Secrets.secret1) => Success(SessionId(Injection.long2BigEndian.invert(t._1).get, t._2)(Secrets.secret1))
    case t if SessionCrypto.validateWithSecret(t)(Secrets.secret2) => Success(SessionId(Injection.long2BigEndian.invert(t._1).get, t._2)(Secrets.secret2))
    case _ => Failure(new Exception("Signature does not correspond to any available secret"))
  }

  implicit lazy val sessionId2Bytes: Bijection[SessionId, Array[Byte]] =
    new AbstractBijection[SessionId, Array[Byte]] {
      def apply(id: SessionId): Array[Byte] = id.sigPayload ++ id.sig

      override def invert(bytes: Array[Byte]): SessionId = {
        (for {
          tuple <- parseBytes(bytes)
          sessionId <- sessionIdWithSecrets(tuple)
          if sessionId.valid(tuple._3)
        } yield sessionId).get
      }
    }
  implicit lazy val session2String = Injection.connect[SessionId, Array[Byte], Base64String, String]

}
