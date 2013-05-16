package solutions

import tools.TimeTool

object Problem44 {
  val limit = 3000
  lazy val numbers = (1 to limit).map(n => (n * (3 * n - 1) * 0.5).toInt)
  lazy val reverseNumbers = numbers.toSet

  def pn(n: Int) = numbers(n - 1)
  def isPentangonal(n: Int) = reverseNumbers.contains(n)

  def main(args: Array[String]) {
    TimeTool.start

    val r = (for {
      distance <- 1 until limit
      left <- 1 to (limit - distance)
      ln = pn(left)
      rn = pn(left + distance)
      if (isPentangonal(rn - ln) && isPentangonal(ln + rn))
    } yield {
      s"$left - ${left + distance} - $rn - $ln - ${rn - ln}"
    }).take(1).head

    println(r)

    TimeTool.printAndStop
  }
}
