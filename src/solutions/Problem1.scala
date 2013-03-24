package solutions

object Problem1 {
  def main(args: Array[String]) {
    val result = (1 until 1000).map { (i:Int) =>
      if (i % 3 == 0 || i % 5 == 0) i else 0
    }.sum

    println(result)
  }
}
