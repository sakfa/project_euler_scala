package pl.sakfa.project_euler.solutions;

import math.BigInt
import com.github.nscala_time.time.Imports._
import org.joda.time.DateTimeConstants;

object Problem19 {
  def main(args: Array[String]) {

    val fromToToIter = new Iterator[LocalDate] {
      var current = new LocalDate(1900, 12, 31);
      val to = new LocalDate(2000, 12, 31);

      def hasNext: Boolean = {
        to.compareTo(current) > 0;
      };
      def next = {
        current = current + 1.days;
        current
      };
    }

    var counter = 0;
    val sundaysOnFirstDayOfMonth = for (cur <- fromToToIter) {
      if (cur.dayOfWeek.get == DateTimeConstants.SUNDAY &&
          cur.dayOfMonth.get == 1) {
        counter += 1;
      }
    }

    println(counter);
  }
}
