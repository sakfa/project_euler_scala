package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.tools.{DigitHelper, TimeTool}

/**
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39  186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
 */
object Problem32 {
  def isPandigital(multiplicand: Int, multiplier: Int, product: Int) = {
    DigitHelper.is9Pandigital(multiplicand.toString + multiplier.toString + product.toString)
  }

  def main(args: Array[String]) {
    TimeTool.start()
    val products = for {
      product <- 1000 to 9999
      multiplicand <- 1 to product
      if (product % multiplicand == 0)
      if (isPandigital(multiplicand, product / multiplicand, product))
    } yield product

    println {
      products.distinct.sum
    }

    TimeTool.printAndStop()
  }
}
