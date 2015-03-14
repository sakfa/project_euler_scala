package pl.sakfa.project_euler.solutions;

import pl.sakfa.project_euler.tools.TimeTool

object Problem15 {
  val n = 20

  //setup grid with bottom row and right column initialized to 1 (zero otherwise)
  val grid = Array.tabulate[Long](n + 1, n + 1) { (x: Int, y: Int) =>
    if (x == n || y == n) 1 else 0
  }

  for (rowIdx <- n - 1 to 0 by -1 ;
       colIdx <- n - 1 to 0 by -1) {
    grid(rowIdx)(colIdx) = grid(rowIdx + 1)(colIdx) + grid(rowIdx)(colIdx + 1)
  }

  def printGrid() {
    for (row <- grid) {
      for (col <- row) {
        print(("% 10d").format(col))
      }
      println() ; println()
    }

  }

  def main(args: Array[String]) {
    TimeTool.start()

    println(grid(0)(0))

    TimeTool.printAndStop()
  }

}
