package pl.sakfa.project_euler.runner

import com.typesafe.scalalogging.slf4j.Logging
import pl.sakfa.project_euler.tools.TimeTool

object SolutionsRunner extends Logging {
  val myClassLoader = getClass.getClassLoader

  def main(args: Array[String]) {

    val bounds = args.map(_.toInt);
    val range =
      if (bounds.length == 0) (1 to 57)
      else (bounds.head to bounds.tail.headOption.getOrElse(bounds.head))

    range.foreach { i =>
      logger.info(s"Running solution for problem $i")
      run(s"pl.sakfa.project_euler.solutions.Problem$i")
    }
  }

  def run(name: String) {
    try {
      val testMainMethod = Class.forName(name, true, myClassLoader).getMethod("main", classOfStringArray)
      TimeTool.start()
      testMainMethod.invoke(null, Array("runner"))
      TimeTool.printAndStop()
    } catch {
      case notFound: java.lang.ClassNotFoundException => logger.error(s"No solution found for problem $name")
      case t: Throwable => logger.error(s"Error while running solution $name", t)
    }
  }

  def classOfStringArray = {
    val clazz = Class.forName("java.lang.String")
    java.lang.reflect.Array.newInstance(clazz,0).getClass
  }
}
