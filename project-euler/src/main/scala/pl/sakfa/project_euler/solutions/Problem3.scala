package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.primes.Factorizer

object Problem3 {
  def greatestPrimeFactor(n: Long) = {
    val factorizer = Factorizer.forGreatestPrimeFactorLimitedTo(math.sqrt(n).toInt + 1)
    factorizer.factorize(n).keySet.max
  }

  def main(args: Array[String]) {
    println(greatestPrimeFactor(600851475143L))
  }
}
