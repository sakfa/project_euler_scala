package solutions

object Problem4 {
  def isProductOf3DigitsNumbers(n: Int): Boolean = {
    for (firstFactor <- 100 to 999 if n % firstFactor == 0) {
      val secondFactor = n / firstFactor
      if (secondFactor < 1000 && secondFactor >= 100) {
        return true
      }
    }

    false
  }

  def isPalindromic(n: Int) = n.toString == n.toString.reverse

  def main(args: Array[String]) {
    for (i <- 999999 to 100 by -1 if isPalindromic(i) if isProductOf3DigitsNumbers(i)) {
      println(i)
      return
    }
  }
}
