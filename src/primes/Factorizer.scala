package primes

import annotation.tailrec

class Factorizer(val limit: Int) {
  lazy val primes = SieveOfAtkin(limit).listPrimes()

  def factorize(n: Long): Map[Int, Int] = {
    var acc = n
    val result = scala.collection.mutable.Map[Int, Int]()

    while (acc > 1) {
      @tailrec def firstPrimeDivisor(x: Int): Int =
        if (acc % primes(x) == 0) primes(x).toInt
        else firstPrimeDivisor(x + 1)

      val primeDivisor: Integer = firstPrimeDivisor(0)
      result.put(primeDivisor, result.getOrElse[Int](primeDivisor, 0) + 1)
      acc = acc / primeDivisor
    }

    result.toMap
  }
}


