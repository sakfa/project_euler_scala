package solutions

import tools.TimeTool

object Problem22 {

  def alphabeticSorter(x: String, y: String): Boolean = (x compareToIgnoreCase y) < 0

  val names = {
    io.Source.fromFile("src/data/problem22_names.txt").getLines().mkString
      .split(",")
      .map(_.replaceAll("\"", ""))
      .sortWith(alphabeticSorter)
      .toList
  }

  def nameValue(n: String) = n.map(_.toInt - 'A' + 1).sum

  def main(args: Array[String]) {
    TimeTool.start
    val scores = names.zipWithIndex.map { case (name, index) => (index + 1) * nameValue(name) }
    println {
      (0L /: scores) { case (acc, score) => acc + score }
    }

    TimeTool.printAndStop
  }

}
