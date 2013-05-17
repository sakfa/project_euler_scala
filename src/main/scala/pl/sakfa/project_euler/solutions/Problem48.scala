package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.tools.TimeTool

object Problem48 {
  def main(args: Array[String]) {
    TimeTool.start()

    val mod = BigInt("10000000000")
    val result = (1 to 1000).map(i => BigInt(i).pow(i)).sum
    println(result % mod)

    TimeTool.printAndStop()
  }
}
