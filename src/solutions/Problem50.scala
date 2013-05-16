package solutions

import primes.SieveOfAtkin
import tools.TimeTool

object Problem50 {
  val limit = 1000000
  val maxseq = 540
  val minseq = 21

  lazy val sieve = SieveOfAtkin(1000000)
  def isPrime(n: Int) = sieve.isPrime(n)
  val primes = (1 to limit).filter(isPrime(_)).toList

  def lookForStartingWithPrimeAt(idx: Int) = {
    var currentSum = 0
    var currentLength = 0
    var maxLength = 0
    var maxPrime = 0

    primes.view(idx, primes.length).takeWhile { p =>
      currentSum <= 1000000
    } foreach { p =>
      currentSum += p
      currentLength += 1

      if (currentSum < 1000000 && isPrime(currentSum)) {
        maxLength = currentLength
        maxPrime = currentSum
      }
    }

    (maxLength, maxPrime, primes.view(idx, idx + currentLength))
  }


  def main(args: Array[String]) {
    TimeTool.start

    val res = (0 until primes.length).par.map(i => lookForStartingWithPrimeAt(i)).maxBy(_._1)
    println(res._1, res._2, res._3.toList)

    TimeTool.printAndStop
  }
}
