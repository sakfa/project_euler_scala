package solutions

import primes.Factorizer
import tools.TimeTool

object Problem3 {
  def greatestPrimeFactor(n: Long) = {
    val factorizer = Factorizer.forFactorizedNumberLimitedTo(n.toInt)
    factorizer.factorize(n).keySet.max
  }

  def main(args: Array[String]) {
    TimeTool.start

    println(greatestPrimeFactor(600851475143L))

    TimeTool.printAndStop
  }
}
