package pl.sakfa.project_euler.solutions

import collection.mutable

object Problem24 {
  var lookup = mutable.Map[Int,Int]()
  def f(n: Int): Int = lookup.getOrElseUpdate(n, n match {
    case 1 => 1
    case _ => n * f(n - 1)
  })

  val limit = 1000000
  val permutationSize = 10
  var permutation = (0 to permutationSize - 1).toArray

  def swap(i1: Int, i2: Int) {
    val tmp = permutation(i1)
    permutation(i1) = permutation(i2)
    permutation(i2) = tmp
  }
  def main(args: Array[String]) {
    var remaining = limit - 1

    for (i <- 0 until permutationSize - 1) {
      val curF = f(permutationSize - i - 1)
      val switches = remaining / curF
      remaining = remaining % curF

      swap(i, i+switches)
      permutation = permutation.patch(i+1,permutation.slice(i+1, permutationSize).sortWith(_ < _), permutationSize - i - 1)
    }

    permutation.foreach(print _)
    println()
  }
}
