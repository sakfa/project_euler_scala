package primes

import annotation.tailrec

object SieveOfEratosthenes {
  def apply(n: Int) = {
    new SieveOfEratosthenes(n)
  }
}

class SieveOfEratosthenes(val limit: Int) extends Sieve {
  require(limit > 5, "this class is optimized for sieves for at least 5 primes")

  override lazy val sieve: Array[Boolean] = {
    //initialize sieve (we are marking multiplies of 2, 3 and 5 as non-prime at init time to optimize a bit)
    val tmpSieve = Array.tabulate[Boolean](limit + 1)( (x: Int) => !(x % 2 == 0 || x % 3 == 0 || x % 5 == 0) )

    //correct values for limit <= 5 (0 and 1 are non prime by definition, 2 3 and 5 were
    tmpSieve(0) = false ; tmpSieve(1) = false
    tmpSieve(2) = true ;  tmpSieve(3) = true ; tmpSieve(5) = true

    //we will sieve limit <= square root of limit
    val sieveLimit = scala.math.sqrt(limit).floor.toInt - 1
    @tailrec def sieveNext(current: Int): Unit = {
      if (tmpSieve(current)) {
        for (i <- current * 2 to limit by current) {
          tmpSieve(i) = false
        }
      }
      if (current <= sieveLimit) {
        sieveNext(current + 1)
      }
    }

    sieveNext(7)
    tmpSieve
  }
}
