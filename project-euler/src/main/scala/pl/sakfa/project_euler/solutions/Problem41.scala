package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.primes.PrimeEstimations

object Problem41 {
  def isPrime(i: Int): Boolean = PrimeEstimations.millerRabinTest(i)

  def main(args: Array[String]) {
    val streams = (9 to 1 by -1) map (i => 1 to i) map ( _.reverse.permutations.toStream )
    val pandigitalDescending = (streams foldRight Stream.empty[IndexedSeq[Int]]) { _ #::: _ }

    for (perm: IndexedSeq[Int] <- pandigitalDescending ; num = perm.mkString("").toInt
      if isPrime(num)) {
      println(num)
      return
    }
  }
}

