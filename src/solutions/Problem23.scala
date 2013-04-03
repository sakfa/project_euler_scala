package solutions

import primes.{Sigma, Factorizer}
import tools.TimeTool
import collection.mutable

/**
A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
 */
object Problem23 {
  implicit val factorizer = Factorizer.forGreatestPrimeFactorLimitedTo(100000)
  def sigma1 = Sigma.sigmaX(1) _
  def properDivisors(n: Int) = sigma1(n) - n

  val abundantNumbersLookup = mutable.Map[Int, Boolean]()

  def isAbundant(i: Int) = abundantNumbersLookup.getOrElseUpdate(i, properDivisors(i) > i)
  val abundantNumbersSequence = (1 to 28123).filter(isAbundant(_))

  def isWritableAsSumOfTwoAbundantNumbers(i: Int): Boolean = {
    abundantNumbersSequence.takeWhile(_ < i).foreach( (j:Int) =>
      if (isAbundant(i - j)) return true
    )

    false
  }

  def main(args: Array[String]) {
    TimeTool.start
    println {
      (1 to 23).sum + (25 to 28123).filter(!isWritableAsSumOfTwoAbundantNumbers(_)).sum
    }
    TimeTool.printAndStop
  }
}
