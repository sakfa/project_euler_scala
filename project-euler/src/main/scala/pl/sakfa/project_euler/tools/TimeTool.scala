package pl.sakfa.project_euler.tools

import com.typesafe.scalalogging.slf4j.Logging

object TimeTool extends Logging {
  var time: Option[Long] = None

  def start() {
    if (time == None) {
      logger.debug(" * starting timer *")
      time = Some(System.nanoTime())
    }
  }

  def printCurrent(header: String) {
    print(header + " - ")
    printCurrent()
  }

  def printCurrent() {
    logger.info(time match {
      case Some(nanos) => "duration: " + formatNanos(System.nanoTime() - nanos)
      case None => "Timer has not been started, yet"
    })
  }

  def stop() {
    time = None
  }

  def printAndStop() {
    if (time != None) {
      printCurrent()
      stop()
    }
  }

  def formatNanos(nanos: Long) = {
    val millis = nanos / 1000000

                  //dont do this in production code :)
    implicit def hackyTupleExpansionToProvideDefaultValueInLegacyCodeWhichShowsWhyScalaIsAwesomeAndHowYouCanAbuseTheLanguageToItsLimits
                  (i: (Long,String)): (Long,String,Long) = (i._1, i._2, 0L)

    var toPrint = List[(Long, String, Long)]()
    toPrint ::= (millis % 1000, "ms", 3)
    toPrint ::= (millis / (1000) % 60, "s")
    toPrint ::= (millis / (1000 * 60) % 60, "m")
    toPrint ::= (millis / (1000 * 60 * 60) % 24, "h")
    toPrint ::= (millis / (1000 * 60 * 60 * 24) % 7, "d")
    toPrint ::= (millis / (1000 * 60 * 60 * 24) / 7, "w")

    while (toPrint != Nil && toPrint.head._1 == 0)
      toPrint = toPrint.tail

    toPrint match {
      case Nil => "0ms"
      case _ => {
        var concatenated = ""
        toPrint.foreach { arg: (Long, String, Long) =>
          concatenated =
              concatenated +
              (if (arg._3 > 0 && concatenated.length() > 0)
                  ("%0" + arg._3 + "d").format(arg._1)
               else arg._1) +
              arg._2
        }
        concatenated
      }
    }
  }
}
