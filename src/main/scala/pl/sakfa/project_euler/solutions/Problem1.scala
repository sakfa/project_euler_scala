package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.tools.TimeTool

object Problem1 {
  def main(args: Array[String]) {
    val result = (1 until 1000).filter(i => i % 3 == 0 || i % 5 == 0).sum
    println(result)
  }
}
