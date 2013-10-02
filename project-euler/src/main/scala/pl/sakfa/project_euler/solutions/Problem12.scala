package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.primes.Factorizer
import pl.sakfa.project_euler.tools.TimeTool
import annotation.tailrec

object Problem12 {
  private class TriangleNumberIterator extends Iterator[Long] {
    var idx = 0
    var acc = 0
    def hasNext: Boolean = true
    def next(): Long = {
      idx += 1
      acc += idx
      acc
    }
  }

  val factorizer = Factorizer.forGreatestPrimeFactorLimitedTo(10000000)

  def divisors(factors: Map[Int, Int]) = {
    factors.values.map(_ + 1).product
  }

  def main(args: Array[String]) {
    val iter = new TriangleNumberIterator


    TimeTool.start()
    @tailrec def search(current: Long): Long = {
        val sigma0 = divisors(factorizer.factorize(current))
        if (sigma0 > 500) current
        else search(iter.next())
    }

    println(search(iter.next()))
    TimeTool.printAndStop()
  }
}
