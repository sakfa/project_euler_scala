package primes

trait Sieve {
  val sieve: Array[Boolean]

  def listPrimes(): Seq[Long] = {
    for (i <- sieve.indices if sieve(i)) yield i.toLong
  }

  def isPrime(i: Int) = i > 0 && sieve(i)
}
