package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.primes.SieveOfAtkin
import pl.sakfa.project_euler.tools.TimeTool

object Problem51 {
  val sieve = SieveOfAtkin(1000000)
  def isPrime(n: Int) = sieve.isPrime(n)

  def indicesOfDigit(subject: String, digit: Int): List[Int] = {
    require(digit >= 0 && digit < 10)
    indicesOfDigit(subject, (digit + 0x30).toChar)
  }
  def indicesOfDigit(subject: String, digit: Char): List[Int] = {
    subject match {
      case "" => Nil
      case _ => {
        val nextIdx = subject.indexOf(digit)
        if (nextIdx == -1) Nil else nextIdx :: (indicesOfDigit(subject.substring(nextIdx + 1), digit) map (i => i + nextIdx + 1))
      }
    }
  }

  // from http://stackoverflow.com/questions/11581175/how-to-generate-the-power-set-of-a-set-in-scala
  def powerSet[A](s: List[A]): List[List[A]] = {
    def pwr(s: List[A], acc: List[List[A]]): List[List[A]] = s match {
      case Nil => acc
      case a :: as => pwr(as, acc ::: (acc map (a :: _)))
    }
    pwr(s, Nil :: Nil)
  }

  def findSameDigitsCombinations(n: Int): List[List[Int]] = {
    val ns = n.toString
    (0 to 9).map(d => indicesOfDigit(ns, d).reverse).flatMap(indices => powerSet(indices)).filterNot(x => x == Nil).toList
  }

  def replaceDigits(n: Int, indices: List[Int], d: Int): Int = {
    val builder = new StringBuilder(n.toString)
    val targetDigit = (d + 0x30).toChar
    indices.foreach { idx =>
      builder(idx) = targetDigit
    }

    builder.toString().toInt
  }

  def primeNumberFamily(prime: Int, indicesToReplace: List[Int]): Seq[Int] = {
    for {
      digit <- 0 to 9
      if (!(digit == 0 && indicesToReplace.head == 0))
      replaced = replaceDigits(prime, indicesToReplace, digit)
      if (isPrime(replaced))
    } yield replaced
  }

  def maxPrimeValueFamilyLength(prime: Int) = {
    val digitsIndices = findSameDigitsCombinations(prime)
    (for (indices <- digitsIndices) yield primeNumberFamily(prime, indices)).map(_.length).max
  }

  def main(args: Array[String]) {
    val primes = sieve.listPrimes()

    TimeTool.start()

    val result = primes.find(p => {
      maxPrimeValueFamilyLength(p.toInt) == 8
    })

    TimeTool.printAndStop()

    println(result)
  }
}

