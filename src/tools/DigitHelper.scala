package tools

object DigitHelper {
  def countOfDigits(n: Int): Int = n match {
    case i if i < 10 => 1
    case i if i < 100 => 2
    case i if i < 1000 => 3
    case i if i < 10000 => 4
    case i if i < 100000 => 5
    case i if i < 1000000 => 6
    case i if i < 10000000 => 7
    case i if i < 100000000 => 8
    case i if i < 1000000000 => 9
    case _ => 10
  }

  def is9Pandigital(num: String) = {
    num.length == 9 && num.sortBy(_.toInt) == "123456789"
  }
}
