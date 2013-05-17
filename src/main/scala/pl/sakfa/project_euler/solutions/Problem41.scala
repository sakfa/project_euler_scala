package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.primes.SieveOfAtkin

object Problem41 {
  val sieve = SieveOfAtkin(1000000000)

  def isPrime(i: Int): Boolean = sieve.isPrime(i)

  def main(args: Array[String]) {
    val streams = (9 to 1 by -1) map (i => 1 to i) map ( _.reverse.permutations.toStream )
    val pandigitalDescending = (streams foldRight Stream.empty[IndexedSeq[Int]]) { _ #::: _ }

    for (perm: IndexedSeq[Int] <- pandigitalDescending ; num = perm.mkString("").toInt if isPrime(num)) {
      println(num)
      return
    }
  }
}

