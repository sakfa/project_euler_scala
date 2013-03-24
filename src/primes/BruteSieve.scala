package primes

import annotation.tailrec

object BruteSieve {
  def apply(limit: Int) = {
    new BruteSieve(limit)
  }
}

class BruteSieve(val limit: Int) extends Sieve{
  lazy val sieve: Array[Boolean] = {
    findPrimes()
  }

  def findPrimes(): Array[Boolean] = {
    val isPrime = Array.tabulate[Boolean](limit + 1)( (x: Int) => !(x % 2 == 0 || x % 3 == 0 || x % 5 == 0) )
    //correct values for limit <= 5 (0 and 1 are non prime by definition, 2 3 and 5 were
    isPrime(0) = false ; isPrime(1) = false
    isPrime(2) = true ;  isPrime(3) = true ; isPrime(5) = true

    for (i <- 6 to limit) {
      val sqrt = scala.math.sqrt(i).toInt

      @tailrec def divisorExists(current:Int): Boolean = {
        if (i % current == 0)
          true
        else if (current > sqrt)
          false
        else
          divisorExists(current + 1)
      }

      isPrime(i) = !divisorExists(2)
    }
    isPrime
  }
}
