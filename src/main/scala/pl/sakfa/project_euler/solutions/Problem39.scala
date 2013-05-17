package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.tools.TimeTool

/**
If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p  1000, is the number of solutions maximised?
 */
object Problem39 {

  def listRightAngleTriangles(p: Int): Seq[(Int, Int, Int)] = {
    for {
      a <- (1 to p)
      b <- (a to p - a)
      c = p - b - a
      if (c >= b)
      if (a + b > c)
      if (a * a + b * b == c * c)
    } yield (a,b,c)
  }

  def countSolutions(p: Int): Int = listRightAngleTriangles(p).length

  def main(args: Array[String]) {
    TimeTool.start()

    println {
      (for (p <- 1 to 1000) yield (p, countSolutions(p))).maxBy(_._2)._1
    }

    TimeTool.printAndStop()
  }
}
