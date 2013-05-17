package pl.sakfa.project_euler.solutions

/**
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
 */
object Problem34 {
  def factorial(n: Int): Int = n match {
    case 0 | 1 => 1
    case _ => n * factorial(n - 1)
  }
  val lowFactorials = (0 to 9).map(factorial(_)).toArray

  def main(args: Array[String]) {
    var sum = 0
    for (i <- 10 to 7 * lowFactorials(9)) {  //upper bound taken from forum after solving for infinite stream and 2 minutes of calculations ;)
      if (i.toString.map(_.toInt - 0x30).map(lowFactorials(_)).sum == i) {
        sum += i
        println(i + " - " + sum)
      }
    }
  }
}
