package solutions

import tools.TimeTool

object Problem28 {
  def sqr(x: Long) = x * x
  def sumOfDiagonals(side: Long): Long = {
    require (side % 2 == 1, "I have made formula only for odd numbers ;)")

    side match {
      case 1 => 1
      case n =>
        (sqr(n-2) + 2 * (n/2)) * 4 + 6 * (n - 1) + sumOfDiagonals(n - 2)
    }
  }

  def main(args: Array[String]) {
    TimeTool.start
    println(sumOfDiagonals(1001))
    TimeTool.printAndStop
  }
}
