package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.primes.SieveOfAtkin

object Problem10 {
  def main(args: Array[String]) {
    val TWO_MILLIONS = 2000000
    val primes = SieveOfAtkin(TWO_MILLIONS).listPrimes()

    val sum = primes.takeWhile(_ < TWO_MILLIONS).sum

    println(sum)
  }
}
