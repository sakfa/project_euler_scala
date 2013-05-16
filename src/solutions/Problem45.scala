package solutions

import primes.NGonalNumbers._
import tools.TimeTool

object Problem45 {
  def main(args: Array[String]) {
    TimeTool.start

    val result = Stream.from(1).map(tn(_)).filter(tn => isPn(tn) && isHn(tn)).take(3)
    println(result.toList)

    TimeTool.printAndStop
  }
}
