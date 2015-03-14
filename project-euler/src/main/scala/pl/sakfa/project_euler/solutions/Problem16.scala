package pl.sakfa.project_euler.solutions;

import math.BigInt

object Problem16 {
  def main(args: Array[String]) {
    def sumdigits(n: BigInt) = {
      n.toString().map { c: Char =>
        c.toInt - 0x30
      }.sum
    }

    println(sumdigits(BigInt(2).pow(1000)))
  }
}
