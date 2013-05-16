package solutions

import tools.TimeTool

object Problem1 {
  def main(args: Array[String]) {
    TimeTool.start

    val result = (1 until 1000).filter(i => i % 3 == 0 || i % 5 == 0).sum

    println(result)

    TimeTool.printAndStop
  }
}
