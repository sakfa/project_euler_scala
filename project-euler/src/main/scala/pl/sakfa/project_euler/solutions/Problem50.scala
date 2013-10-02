package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.primes.SieveOfAtkin
import pl.sakfa.project_euler.tools.TimeTool

object Problem50 {
  val limit = 1000000

  lazy val sieve = SieveOfAtkin(limit)
  def isPrime(n: Int) = sieve.isPrime(n)
  val primes = (1 to limit).filter(isPrime(_)).toList

  def lookForStartingWithPrimeAt(idx: Int) = {
    var currentSum = 0
    var currentLength = 0
    var maxLength = 0
    var maxPrime = 0

    primes.view(idx, primes.length).takeWhile { p =>
      currentSum += p
      currentSum <= limit
    } foreach { p =>
      currentLength += 1

      if (isPrime(currentSum)) {
        maxLength = currentLength
        maxPrime = currentSum
      }
    }

    (maxLength, maxPrime, primes.view(idx, idx + currentLength))
  }


  var currentMax = (0, 0, primes.view(0,0))
  def main(args: Array[String]) {
    TimeTool.start()


    Stream.from(0).takeWhile { i =>
    /** the idea behind this condition is simple: if current prime multiplied by current longest streak is greater than
      *   limit then we will certainly NOT find solution here, nor in any prime greater than this one */
      primes(i) * currentMax._1 < limit
    } foreach { i =>
      val newValue = lookForStartingWithPrimeAt(i)
      if (newValue._1 > currentMax._1) {
        currentMax = newValue
      }
    }

    println(s"longest chain size ${currentMax._1}, from ${currentMax._3.head} to ${currentMax._3.last}, summing up to ${currentMax._2}")

    TimeTool.printAndStop()
  }
}
