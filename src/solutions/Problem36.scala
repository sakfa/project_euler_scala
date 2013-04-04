package solutions

object Problem36 {
  def main(args: Array[String]) {
    println{
      (1 until 1000000)
      .filter(i => i.toString == i.toString.reverse && i.toBinaryString == i.toBinaryString.reverse)
      .sum

    }
  }
}
