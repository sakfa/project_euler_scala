package primes

object SieveOfAtkin {
  def apply(n: Int) = {
    new SieveOfAtkin(n)
  }
}

class SieveOfAtkin(val limit: Int) extends Sieve {
  require(limit > 5, "this class is optimized for sieves for at least 5 primes")

  override lazy val sieve = {
    findPrimes()
  }

  def init() {
    sieve
  }

  private def findPrimes(): Array[Boolean] = {
    println("(calculating sieve of atkin up to n = " + limit + ")")
    val isPrime = Array.fill[Boolean](limit + 1)(false)
    val sqrtOfLimit = scala.math.sqrt(limit).floor.toInt

    for (x <- 1 to sqrtOfLimit ; y <- 1 to sqrtOfLimit) {
      val xsqr: Int = x * x
      val ysqr: Int = y * y
      var n = 4 * xsqr + ysqr
      if (n <= limit && (n % 12 == 1 || n % 12 == 5)) {
        isPrime(n) = !isPrime(n)
      }

      n = 3 * xsqr + ysqr
      if (n <= limit && n % 12 == 7) {
        isPrime(n) = !isPrime(n)
      }

      n = 3 * xsqr - ysqr
      if ( (x > y) && (n <= limit) && (n % 12 == 11)) {
        isPrime(n) = !isPrime(n)
      }
    }

    for (n <- 5 to sqrtOfLimit) {
      if (isPrime(n)) {
        val squareOfN = n * n
        for (m <- squareOfN to limit by squareOfN) {
          isPrime(m) = false
        }
      }
    }

    isPrime(2) = true
    isPrime(3) = true
    println("(sieve calculation ended")
    isPrime
  }


}

