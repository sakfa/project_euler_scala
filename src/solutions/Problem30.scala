package solutions

import tools.TimeTool

object Problem30 {
  val powers = (0 to 9).map(math.pow(_, 5).toInt).toArray
  val upperBound = powers(9) * 6

  def sumOfPowers(n: Int) = n.toString.map(c => powers(c.toInt - 0x30)).sum

  def main(args: Array[String]) {
    TimeTool.start

    println {
      (2 to upperBound)
        .filter(n => n == sumOfPowers(n))
        .sum
    }

    TimeTool.printAndStop
  }
}
