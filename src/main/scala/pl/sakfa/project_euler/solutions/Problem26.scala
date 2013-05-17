package pl.sakfa.project_euler.solutions

import collection.mutable.ArrayBuffer
import collection.mutable
import pl.sakfa.project_euler.tools.TimeTool

object Problem26 {

  def recurringCycle(denominator: Int): Int = {
    var resultDigits = ArrayBuffer[Int]()
    var nominator = 1
    var currentStep = 0
    val nominatorLookup = mutable.Map[Int,Int]()

    while (nominator != 0) {
      val newDigit = nominator / denominator
      resultDigits += newDigit

      nominator = (nominator - newDigit * denominator) * 10

      //if we have already seen this nominator then we got our cycle
      nominatorLookup.get(nominator) match {
        case Some(nominatorStep) => return currentStep - nominatorStep
        case None => nominatorLookup.put(nominator, currentStep)
      }

      currentStep += 1
    }

    //division completed without detecting any cycle
    0
  }

  def main(args: Array[String]) {
    TimeTool.start()

     println {
       (1 to 1000).map { i: Int =>
         (i, recurringCycle(i))
       }.maxBy(_._2)
     }

    TimeTool.printAndStop()
  }
}
