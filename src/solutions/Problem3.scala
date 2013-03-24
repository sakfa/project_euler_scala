package solutions

import primes.Factorizer
import tools.TimeTool

object Problem3 {
  def greatestPrimeFactor(n: Long) = {
    val factorizer = new Factorizer(math.sqrt(n).ceil.toInt)
    factorizer.factorize(n).keySet.max
  }

  def main(args: Array[String]) {
    TimeTool.start

    println(greatestPrimeFactor(600851475143L))

    TimeTool.printAndStop
  }
}
