package solutions

import math.ScalaNumber

class FibonnacciIterator extends Iterator[BigInt] {
  var prev = BigInt(0)
  var prevprev = BigInt(1)

  override val hasDefiniteSize = false
  override val hasNext = true
  def next(): BigInt = {
    val res = prevprev + prev
    prevprev = prev
    prev = res
    res
  }
}

object Problem25 {
  def main(args: Array[String]) {
    val iter = new FibonnacciIterator
    println {
      iter.takeWhile(_.toString().length < 1000).length + 1
    }
  }
}
