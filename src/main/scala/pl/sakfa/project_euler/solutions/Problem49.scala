package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.primes.SieveOfAtkin
import pl.sakfa.project_euler.tools.TimeTool

object Problem49 {
  val sieve = SieveOfAtkin(10000)
  sieve.init()
  def isPrime(i: Int) = sieve.isPrime(i)

  def isEachOtherPermutation(i1: Int, i2: Int, i3: Int) = {
    var base = i1.toString.sorted
    (base == i2.toString.sorted) &&
    (base == i3.toString.sorted)
  }

  def main(args: Array[String]) {
    TimeTool.start()

    val primes = for {
      i <- (1000 to 9997)
      if (isPrime(i))
      a <- 1 to ((9999 - i) / 2)
      t1 = i
      t2 = i + a
      if (isPrime(t2))
      t3 = t2 + a
      if (isPrime(t3))
      if (isEachOtherPermutation(t1, t2, t3))
    } yield (t1.toString + t2 + t3)

    println(primes)

    TimeTool.printAndStop()
  }
}
