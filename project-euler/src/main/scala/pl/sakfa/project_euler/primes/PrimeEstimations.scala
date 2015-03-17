package pl.sakfa.project_euler.primes

import math._

object PrimeEstimations {
  def nthPrimeLowerBound(n: Int): Int = {
    if (n < 6) {
      1
    } else {
      val logn = log(n)
      val r = n * ( logn + log(logn) - 1)
      r.floor.toInt
    }
  }

  def nthPrimeUpperBound(n: Int): Int = {
    if (n < 6) {
      13
    } else {
      val logn = log(n)
      val r = n * ( logn + log(logn))
      r.ceil.toInt
    }
  }

  //will return true if n is prime
  // yes I know that miller rabin is implemented in java standard library but it would be too
  // lame to use that
  // algorithm and magic numbes  taken from http://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
  def millerRabinTest(n: BigInt) = {
    if (n == 2 || n == 3 || n == 5 || n == 7 || n == 11) true
    else if (n <= 12) false
    else {
      //decomposes n to number in format 2^s * d, returns (s, d)
      def decompose(n: BigInt, acc: Int = 0): (Int, BigInt) = {
        if (n % 2 == 1) (acc, n)
        else decompose(n / 2, acc + 1);
      }
      def isWitness(a: BigInt, s: Int, d: BigInt) = {
        (a.modPow(d, n) != 1) && ((0 until s) forall { r =>
            a.modPow(BigInt(2).pow(r) * d, n) != n - 1;
        })
      }

      val (s, d) = decompose(n - 1);
      val as =
        if (n < 2047) List(2)
        else if (n < 1373653) List(2, 3)
        else if (n < BigInt("4759123141")) List(2, 7, 61)
        else if (n < BigInt("1122004669633")) List(2, 13, 23, 1662803)
        else if (n < BigInt("2152302898747")) List(2, 3, 5, 7, 11)
        else if (n < BigInt("3474749660383")) List(2, 3, 5, 7, 11, 13)
        else if (n < BigInt("341550071728321")) List(2, 3, 5, 7, 11, 13, 17)
        else if (n < BigInt("3825123056546413051")) List(2, 3, 5, 7, 11, 13, 17, 19, 23)
        else throw new Exception("n = $n can't be deterministacaly checked with this test.")

      List(2, 7, 61)

      !as.exists { a =>
        isWitness(a, s, d)
      }
    }
  }
}
