package pl.sakfa.project_euler.primes

import math.log

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
}
