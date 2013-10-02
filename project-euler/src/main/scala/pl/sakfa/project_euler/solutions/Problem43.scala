package pl.sakfa.project_euler.solutions

object Problem43 {
  def isValid(seq: IndexedSeq[Int]): Boolean = {
    val exp = List(
      (1, 2), (2, 3), (3, 5), (4, 7), (5, 11), (6, 13), (7, 17)
    )

    exp.forall { case (s, prime) =>
      (seq(s) * 100 + seq(s + 1) * 10 + seq(s + 2)) % prime == 0
    }
  }

  def main(args: Array[String]) {
    val valid =
      for (permutation <- (0 to 9).permutations if isValid(permutation))
      yield permutation

    println(valid.map(v => BigInt(v.mkString(""))).sum)
  }
}
