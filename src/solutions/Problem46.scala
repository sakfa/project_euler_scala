package solutions

import primes.SieveOfAtkin

object Problem46 {
  val countOfSquares = 10000
  val boundOfPrimes = 1000000

  val squares = (1 to countOfSquares).map(i => i * i)

  val sieve = SieveOfAtkin(boundOfPrimes)
  def isPrime(n: Int) = sieve.isPrime(n)

  def isWritableAsSumOfPrimeAndTwiceSquare(n: Int) = squares.exists(s => isPrime(n - 2 * s))

  def isOddAndComposite(n: Int) = n % 2 == 1 && !isPrime(n) && n > 1

  def main(args: Array[String]) {
    (for (s <- Stream.from(1) if isOddAndComposite(s) && !isWritableAsSumOfPrimeAndTwiceSquare(s)) yield s)
      .take(1).foreach(println)
  }
}
