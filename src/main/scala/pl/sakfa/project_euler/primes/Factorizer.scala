package pl.sakfa.project_euler.primes

import annotation.tailrec

object Factorizer {
  type factors = Map[Int, Int]
  def forFactorizedNumberLimitedTo(limit: Int) = forGreatestPrimeFactorLimitedTo(limit)
  def forGreatestPrimeFactorLimitedTo(limit: Int) = new TrialDivisionFactorizer(math.max(6, limit + 1))
}

trait Factorizer {
  def factorize(n: Long): Factorizer.factors
}

class TrialDivisionFactorizer(val limit: Int) extends Factorizer {
  lazy val primes = SieveOfAtkin(limit).listPrimes()

  def factorize(n: Long): Factorizer.factors = {
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


