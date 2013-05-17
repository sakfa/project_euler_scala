package pl.sakfa.project_euler.runner

import com.typesafe.scalalogging.slf4j.Logging
import pl.sakfa.project_euler.tools.TimeTool

object SolutionsRunner extends Logging {
  val myClassLoader = getClass.getClassLoader

  def main(args: Array[String]) {
    (1 to 51).foreach { i =>
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
      case t: Throwable => logger.error(s"Error while running solution $name", t)
    }
  }

  def classOfStringArray = {
    val clazz = Class.forName("java.lang.String")
    java.lang.reflect.Array.newInstance(clazz,0).getClass
  }
}
