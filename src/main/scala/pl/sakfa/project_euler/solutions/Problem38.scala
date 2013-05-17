package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.tools.{TimeTool, DigitHelper}

/*
Take the number 192 and multiply it by each of 1, 2, and 3:

192  1 = 192
192  2 = 384
192  3 = 576
By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?
 */
object Problem38 {
  def main(args: Array[String]) {
    TimeTool.start()

    val pairs = for ( n <- 2 to 9 ; i <- 1 to 10000) yield (n, i)
    val concatenations = pairs.map{ case (n,i) => (n,i,(for (j <- 1 to n) yield i * j).mkString) }
    println(concatenations.filter{ case (n,i,concat) =>
      DigitHelper.is9Pandigital(concat)
    }.maxBy(_._3))

    TimeTool.printAndStop()
  }
}
