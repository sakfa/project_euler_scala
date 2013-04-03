package solutions

import primes.Factorizer

object Problem5 {
  def mergeMaps(left: Map[Int,Int], right: Map[Int,Int])( op: (Option[Int],Option[Int]) => Int ) = {
    (left.keySet ++ right.keySet).map(i =>
      (i, op(left.get(i), right.get(i)))
    ).toMap
  }

  def mergeFactors(left: Map[Int,Int], right: Map[Int,Int]) = {
    mergeMaps(left, right) { (i:Option[Int],j:Option[Int]) =>
      if (i.getOrElse(0) > j.getOrElse(0))
        i.getOrElse(0)
      else
        j.getOrElse(0)
    }
  }

  //lets do more generic solution
  def smallestNumber(n: Int) = {
    require(n > 1, "Algorithm will fail for n's < 2")
    val factorizer = Factorizer.forGreatestPrimeFactorLimitedTo(n)

    val factors = (factorizer.factorize(2) /: (3 to n)) { case (acc, i) =>
      mergeFactors(acc, factorizer.factorize(i))
    }

    factors.map { entry: (Int,Int) =>
      math.pow(entry._1, entry._2).toInt
    }.product
  }

  def main(args: Array[String]) {
    println(smallestNumber(10))
    println(smallestNumber(20))
  }
}
