package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.tools.TimeTool

object Problem22 {
  def alphabeticSorter(x: String, y: String): Boolean = (x compareToIgnoreCase y) < 0

  val names = {
    io.Source.fromInputStream(getClass.getResourceAsStream("/problem22_names.txt")).mkString
      .split(",")
      .map(_.replaceAll("\"", ""))
      .sortWith(alphabeticSorter)
      .toList
  }

  def nameValue(n: String) = n.map(_.toInt - 'A' + 1).sum

  def main(args: Array[String]) {
    TimeTool.start()
    val scores = names.zipWithIndex.map { case (name, index) => (index + 1) * nameValue(name) }
    println {
      (0L /: scores) { case (acc, score) => acc + score }
    }

    TimeTool.printAndStop()
  }

}
