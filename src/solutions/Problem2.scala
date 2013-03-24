package solutions

import annotation.tailrec

object Problem2 {
  var lookup = scala.collection.mutable.Map[Int, Int]()

  def fibonacci(n: Int): Int = {
    lookup.getOrElseUpdate(n,
      n match {
        case 1 => 1
        case 2 => 2
        case _ => fibonacci(n - 1) + fibonacci(n - 2)
      }
    )
  }

  def main(args: Array[String]) {
    def upperBound = 4000000
    @tailrec
    def sumEvenValued(acc: Int, n: Int): Int = {
      val fib = fibonacci(n)
      if (fib > upperBound) {
        acc
      } else {
        sumEvenValued(acc + (if (fib%2==0) fib else 0), n + 1)
      }
    }

    println(sumEvenValued(0, 1))
  }

}
