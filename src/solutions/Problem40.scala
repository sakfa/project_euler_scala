package solutions

import tools.DigitHelper

object Problem40 {

  def nthDigit(subject: Int, n: Int): Int = subject.toString.charAt(n) - '0'

  def df(n: Int): Int = {
    def internal(leftN: Int, digitCount: Int): Int = {
      val termsStart = math.pow(10, digitCount - 1).toInt
      val termsInCurrentDigitCount = termsStart * 9
      if (leftN <= termsInCurrentDigitCount * digitCount) {
        val whichTerm = (leftN - 1) / digitCount
        val whichDigitOfTerm = (leftN - 1) % digitCount

        nthDigit(whichTerm + termsStart , whichDigitOfTerm)
      } else {
        internal(leftN - termsInCurrentDigitCount * digitCount, digitCount + 1)
      }
    }

    internal(n, 1)
  }

  def main(args: Array[String]) {
    val values = for (exp <- 0 to 6 ; pw = math.pow(10, exp).toInt) yield {
      df(pw)
    }

    println(values.product)
  }

}
