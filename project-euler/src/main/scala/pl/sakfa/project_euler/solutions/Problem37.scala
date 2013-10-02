package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.primes.SieveOfAtkin
import pl.sakfa.project_euler.tools.{TimeTool, DigitHelper}

/**
The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
 */
object Problem37 {
  val primes = new SieveOfAtkin(1000000).sieve
  val powersOfTen = (0 to 7).map(math.pow(10, _).toInt)

  def isPrime(i: Int) = primes(i)

  def isLeftTruncatable(i: Int): Boolean = {
    def loop(curNum: Int, curShift: Int): Boolean = {
      if (curNum == 0) true
      else if (!primes(curNum)) false
      else loop(curNum % powersOfTen(curShift), curShift - 1)
    }

    loop(i, DigitHelper.countOfDigits(i) - 1)
  }

  def isRightTruncatable(i: Int): Boolean = {
    def loop(curNum: Int): Boolean = {
      if (curNum == 0) true
      else if (!primes(curNum)) false
      else loop(curNum / 10)
    }

    loop(i)
  }

  def isTruncatablePrime(i: Int): Boolean = {
    isRightTruncatable(i) && isLeftTruncatable(i)
  }

  def main(args: Array[String]) {
    TimeTool.start()

    var found = List[Int]()
    for (i <- Stream.from(11, step = 2).takeWhile(i => found.length < 11)) {
      if (isTruncatablePrime(i)) found = i :: found
    }

    println(found.sum)
    TimeTool.printAndStop()

  }
}
