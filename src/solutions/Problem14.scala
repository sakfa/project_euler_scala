import collection.mutable.ListBuffer

import collection.mutable
import tools.TimeTool

object Problem14 {
  val results = mutable.Map[Long, Long]()

  def nextLorentz(n:Long) = n match {
    case x if x % 2 == 0 => x / 2
    case x => 3 * x + 1
  }

  def calcSequenceLengths(n: Long) = {

    val res = new ListBuffer[(Long, Option[Long])]()

    var current = (n, results.get(n))
    while (current._1 != 1 && current._2 == None) {
      res.append(current)
      val nl = nextLorentz(current._1)
      current = (nl, results.get(nl))
    }
    res.append(current)

    var lengthBehind = 0L
    val seq = res.reverse.toList.map { x: (Long, Option[Long]) => x._2 match {
      case Some(length) => lengthBehind = length ; (x._1, length)
      case None => lengthBehind = lengthBehind + 1 ; (x._1, lengthBehind)
    }}
    seq foreach { results += _ }
    seq
  }

  def main(args: Array[String]) {

    TimeTool.start

    for (i <- 1 until 1000000) {
      calcSequenceLengths(i)
    }
    println(results.maxBy(_._2))

    TimeTool.printAndStop
  }
}