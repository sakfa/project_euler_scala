package slidingSquare

import scala.util.Random
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import java.util.NoSuchElementException


class Path(val currentState: Board, val steps: List[Action], val stepsLength: Int) {

}
object Path {
  val costOrderingAsc = new Ordering[Path] {
    def compare(x: Path, y: Path): Int = {
      val yc = y.stepsLength + y.currentState.h
      val xc = x.stepsLength + x.currentState.h
      yc.compare(xc)
    }
  }
}

sealed abstract class Action
case object MoveBlankLeft extends Action {
  override def toString = "left"
}
case object MoveBlankRight extends Action {
  override def toString = "right"
}
case object MoveBlankUp extends Action {
  override def toString = "up"
}
case object MoveBlankDown extends Action {
  override def toString = "down"
}

class Board (val size: Int, val state: Array[Int]) {


  override def hashCode(): Int = state.toList.hashCode()
  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that: Board => state.sameElements(that.state)
      case _ => false
    }
  }

  val countOfTiles = size * size

  private lazy val widthOfRenderedTile = (countOfTiles - 1).toString.length
  private def renderTile(tile: Int) = {
    (if (isBlank(tile)) "" else tile.toString).padTo(widthOfRenderedTile, " ").mkString("")
  }
  private def render(): String = {
    def horizontalLine = "".padTo(widthOfRenderedTile * size + size + 1, "-").mkString("")

    val buffer = new StringBuilder
    buffer.append("|")
    for (idx <- 0 until state.length) {
      buffer.append(renderTile(state(idx))).append("|")

      if ((idx + 1) % size == 0) {
        buffer.append("\n")
        if (idx + 1 != countOfTiles) {
          buffer.append(horizontalLine).append("\n|")
        }
      }
    }
    buffer.append("solved: ").append(isSolved).append("\n")
    buffer.append("h: " + h)
    buffer.toString()
  }
  override def toString: String = render

  def isBlank(tile: Int) = tile == countOfTiles
  def xy(idx: Int) = (idx / size, idx % size)
  def unxy(xy: (Int, Int)) = xy._1 * size + xy._2
  lazy val isSolved = {
    (0 until state.length - 1).forall { case i =>
      state(i) == state(i + 1) - 1
    }
  }
  lazy val h = {
    val tileDistances = for {
      i <- 0 until state.length
      if !isBlank(state(i))
    } yield {
      val tile = state(i)
      val tileXY = xy(i)
      val expectedXY = xy(tile - 1)
      Math.abs(tileXY._1 - expectedXY._1) + Math.abs(tileXY._2 - expectedXY._2)
    }
    tileDistances.sum
  }
  lazy val blankPosition =
    state.zipWithIndex
      .find(tile => isBlank(tile._1))
      .map(tile => xy(tile._2))
      .getOrElse(throw new Exception("Corrupt board (no blank?)"))

  lazy val legalActions: List[Action] = {
    var actions = List[Action]()

    val (x,y) = blankPosition
    if (x > 0) actions ::= MoveBlankUp
    if (x < size - 1) actions ::= MoveBlankDown
    if (y > 0) actions ::= MoveBlankLeft
    if (y < size - 1) actions ::= MoveBlankRight
    actions
  }

  def applyAction(action: Action): Board = {
    val nextState = state.clone()

    def swapIdx(p1: Int, p2: Int) = {
      val tmp = nextState(p1) ; nextState(p1) = nextState(p2) ; nextState(p2) = tmp
    }
    def swapXy(p1: (Int, Int), p2: (Int, Int)) = swapIdx(unxy(p1), unxy(p2))
    action match {
      case MoveBlankDown => swapXy(blankPosition, (blankPosition._1 + 1, blankPosition._2))
      case MoveBlankUp => swapXy(blankPosition, (blankPosition._1 - 1, blankPosition._2))
      case MoveBlankLeft => swapXy(blankPosition, (blankPosition._1, blankPosition._2 - 1))
      case MoveBlankRight => swapXy(blankPosition, (blankPosition._1, blankPosition._2 + 1))
    }

    new Board(size, nextState)
  }
}

object Board {
  def goal(size: Int = 4): Board = new Board(size, (1 to (size * size)).toArray)
  def random(size: Int): Board = {
    val result = (1 to (size * size)).toArray
    def swap(i: Int, j: Int) {
      val tmp = result(i)
      result(i) = result(j)
      result(j) = tmp
    }
    val rand = new Random()
    for (idx <- result.length - 1 to 1 by -1) {
      swap(idx, rand.nextInt(idx))
    }
    new Board(size, result)
  }
}

class Frontier {
  val queue = new mutable.PriorityQueue[Path]()(Path.costOrderingAsc)
  val stateLookup = new mutable.HashMap[Board, Path]()

  def +=(path: Path) {
    queue += path
    stateLookup(path.currentState) = path
  }
  def dequeue(): Path = {
    val path = queue.dequeue()
    stateLookup -= path.currentState
    path
  }
  def find(board: Board): Option[Path] = {
    stateLookup.get(board)
  }
}

object Solution {
  def solve(board: Board) = {
    var visited = HashSet[Board]()
    var frontier = new Frontier
    var solved = false
    var unsolvable = false

    frontier += new Path(board, List[Action](), 0)

    var path: Path = null
    do {
      try {
        path = frontier.dequeue()
      } catch {
        case e: NoSuchElementException => {
          unsolvable = true
        }
      }

      if (path.currentState.isSolved) {
        solved = true
      } else if (!unsolvable) {
        visited += path.currentState

        val actions = path.currentState.legalActions
        actions.foreach { action =>
          val nextPath = new Path(path.currentState.applyAction(action), action :: path.steps, path.stepsLength + 1)

          //unless already in explored or contained in frontier with lower cost
          if (!visited.contains(nextPath.currentState)) {
            frontier += nextPath
          }
        }
      }
    } while (!solved && !unsolvable)

    if (unsolvable) {
      println(board)
      println("Sorry, this square is not solvable")
    } else {
      println(board)
      println(path.currentState)
      println(path.steps.reverse)
      println(s"count of steps: ${path.steps.length}")
    }
  }

  def main(args: Array[String]) {
    val size: Int = 2
//    val board = new Board(2, Array[Int](4, 3, 1, 2))//Board.random(size)
    val board = Board.random(size)

    solve(board)
  }
}
