package pl.sakfa.project_euler.solutions

import scala.io.Source
import pl.sakfa.project_euler.primes.NGonalNumbers

object Problem42 {
  def tn(n: Int): Int = NGonalNumbers.tn(n.toLong).toInt
  def isTriangleNumber(n: Int): Boolean = NGonalNumbers.isTn(n)

  val words = Source.fromInputStream(getClass.getResourceAsStream("/problem42_words.txt"))
    .getLines()
    .next()
    .split(",")
    .map(_.replaceAll("\"",""))
    .toVector

  def main(args: Array[String]) {
    val countOfTriangleNumbers =
      words
        .map(w => w.toCharArray.map(c => c.toInt - 'A'.toInt + 1))
        .map(valSeq => valSeq.sum)
        .filter(isTriangleNumber(_))
        .length

    println(countOfTriangleNumbers)
  }
}
