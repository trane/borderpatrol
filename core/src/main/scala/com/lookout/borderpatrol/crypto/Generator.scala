package com.lookout.borderpatrol.crypto

import java.security.SecureRandom

import scala.util.Random

/**
 * Generate random data from a decent PRNG
 */
trait Generator[+A] {
  val random: Random = new Random(new SecureRandom)

  def apply(n: Int): A
}

object Generator {
  import com.lookout.borderpatrol.util.Combinators.tap

  object BytesGenerator extends Generator[Seq[Byte]] {
    def apply(n: Int): Seq[Byte] =
      tap(Array.fill[Byte](n)(0))(random.nextBytes)
  }

  /**
   * Generates `n` bytes of
   * [[com.lookout.borderpatrol.sessionx.Types.Entropy Entropy]]
   */
  object EntropyGenerator extends Generator[Entropy] {
    def apply(n: Int): Entropy =
      BytesGenerator(n).toIndexedSeq
  }

}
