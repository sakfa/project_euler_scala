package pl.sakfa.project_euler.primes

object NGonalNumbers {
  def tn(n: Long): Long = ((0.5 * n) * (n + 1)).toLong
  def pn(n: Long): Long = ((0.5 * n) * (3 * n - 1)).toLong
  def hn(n: Long): Long = n * (2 * n - 1)

  def isTn(n: Long) = solvesInNatural(n, (1, 1, -2 * n))(tn)
  def isPn(n: Long) = solvesInNatural(n, (3, -1, -2 * n))(pn)
  def isHn(n: Long) = solvesInNatural(n, (2, -1, -1 * n))(hn)

  private def solvesInNatural(n: Long, coefficients: (Long, Long, Long))(checkOp: Long => Long): Boolean = {
    val (a,b,c) = coefficients
    val dsqrt = math.sqrt(b * b - 4 * a * c)
    val x1 = ((-b + dsqrt) / (2*a)).toLong
    val x2 = ((-b - dsqrt) / (2*a)).toLong

    (x1 > 0 && checkOp(x1) == n) || (x2 > 0 && checkOp(x2) == n)
  }



  def main(args: Array[String]) {
    assert((1 to 10).map(i => tn(i)).forall(isTn(_)), "OK")
    assert((1 to 10).map(i => tn(i)).forall(i => !isTn(i + 1)), "OK")
    assert((1 to 10).map(i => tn(i)).forall(i => !isTn(i - 1)), "OK")

    assert((1 to 10).map(i => pn(i)).forall(isPn(_)), "OK")
    assert((1 to 10).map(i => pn(i)).forall(i => !isPn(i + 1)), "OK")
    assert((1 to 10).map(i => pn(i)).forall(i => !isPn(i - 1)), "OK")

    assert((1 to 10).map(i => hn(i)).forall(isHn(_)), "OK")
    assert((1 to 10).map(i => hn(i)).forall(i => !isHn(i + 1)), "OK")
    assert((1 to 10).map(i => hn(i)).forall(i => !isHn(i - 1)), "OK")
  }

}
