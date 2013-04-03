package primes

import math._

object Sigma {
  def sigma0(n: Int)(implicit f: Factorizer) = {
    f.factorize(n).values.map(_ + 1).product
  }

  def sigmaX(x: Int)(n: Int)(implicit f: Factorizer) = {
    if (x == 0) sigma0(n)
    else {
      val factors = f.factorize(n)
      factors.map { case (p,a) =>
        ((pow(p, (a+1) * x) - 1) / (pow(p,x) - 1)).toInt
      }.product
    }
  }
}
