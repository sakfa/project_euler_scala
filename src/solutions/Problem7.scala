package solutions

import primes.{PrimeEstimations, SieveOfAtkin}
import tools.TimeTool

object Problem7 {
  def main(args: Array[String]) {
    TimeTool.start

    val primes = SieveOfAtkin(PrimeEstimations.nthPrimeUpperBound(10001)).listPrimes()
    println(primes(10000))

    TimeTool.printAndStop
  }
}
