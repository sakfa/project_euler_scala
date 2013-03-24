package solutions

object Problem6 {
  def diff(n: Int) = {
    val sumOfSquares = (1 to n).map(x => x*x).sum
    val sumToN: Int = (1 to n).sum
    val squareOfSum = sumToN * sumToN
    squareOfSum - sumOfSquares
  }

  def main(args: Array[String]) {
    println("diff: " + diff(100))
  }
}
