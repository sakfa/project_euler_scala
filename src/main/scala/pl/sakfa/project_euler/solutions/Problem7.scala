package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.primes.{PrimeEstimations, SieveOfAtkin}

object Problem7 {
  def main(args: Array[String]) {
    val primes = SieveOfAtkin(PrimeEstimations.nthPrimeUpperBound(10001)).listPrimes()
    println(primes(10000))
  }
}
