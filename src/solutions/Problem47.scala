package solutions

import primes.Factorizer
import tools.TimeTool

object Problem47 {
  val factorizer = Factorizer.forFactorizedNumberLimitedTo(10000000)

  val lookup = collection.mutable.Map[Int, Int]()
  def distinctPrimeFactors(n: Int) = lookup.getOrElseUpdate(n, factorizer.factorize(n).keySet.size)

  def main(args: Array[String]) {
    TimeTool.start

    //very inefficient but sufficient for this problem
    val res = Stream.from(1).sliding(4).find(s => s.forall(x => distinctPrimeFactors(x) == 4))

    println(res)
    TimeTool.printAndStop
  }
}
