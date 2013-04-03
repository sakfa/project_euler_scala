package solutions

import primes.SieveOfAtkin
import tools.TimeTool

/**
 * Euler published the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

Using computers, the incredible formula  n²  79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, 79 and 1601, is 126479.

Considering quadratics of the form:

n² + an + b, where |a|  1000 and |b|  1000

where |n| is the modulus/absolute value of n
e.g. |11| = 11 and |4| = 4
Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.
 */
object Problem27 {
  lazy val sieve = SieveOfAtkin(100000)

  def doesEquaitionYieldPrime(a: Int, b: Int, n: Int) = {
    sieve.isPrime(n * n + a * n + b)
  }

  def main(args: Array[String]) {
    TimeTool.start

    val lengths = for {
        a <- -1000 until 1000
        b <- 1 until 1000
        //following checks condition for n == 0 and n == 1 (spares as some multiplication for simplified equations)
      if (sieve.isPrime(b))
      if (sieve.isPrime(a+b+1))
    } yield {
      val nIter = Stream.from(2).iterator
      (a, b, nIter.takeWhile(doesEquaitionYieldPrime(a, b, _)).length)
    }

    val (a, b, l) = lengths.maxBy(_._3)
    println("a = " + a + ", b = " + b, " l = " + l)
    println("a * b = " + (a * b))
    TimeTool.printAndStop

  }
}
