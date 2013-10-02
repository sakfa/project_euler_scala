package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.primes.SieveOfAtkin
import pl.sakfa.project_euler.tools.TimeTool
import pl.sakfa.project_euler.tools.DigitHelper._

object Problem35 {
  val ONE_MILLION = 1000000
  val primes = new SieveOfAtkin(ONE_MILLION).sieve
  def isPrime(i: Int) = primes(i)

  val powersOfTen = (0 to 7).map(math.pow(10, _).toInt)

  def circles(n: Int): Array[Int] = {
    val nWidth = countOfDigits(n)
    (0 until nWidth).map(i => shiftLeft(n, nWidth, i)).distinct.toArray
  }

  def shiftLeft(n: Int, nWidth: Int, howMuch: Int): Int = {
    require(howMuch <= nWidth)
    val pivotPower = powersOfTen(howMuch)
    val multiplyPower = powersOfTen(nWidth - howMuch)
    val (left, right) = (n / pivotPower, n % pivotPower)
    right * multiplyPower + left
  }

  def main(args: Array[String]) {
    TimeTool.start()

      val circularPrimes =
        (2 until 1000000)
          .map(circles(_))
          .filter(circle => circle.forall(i => primes(i)))
          .flatten
          .distinct

    println(circularPrimes.length)

    TimeTool.printAndStop()
  }
}
