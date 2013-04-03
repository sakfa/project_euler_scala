package solutions

/**
 * The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
 */
object Problem33 {
  case class Problem33Fraction(nominator: Int, denominator: Int) {
    val epsilon = 0.001
    val doubleValue = nominator.toDouble / denominator.toDouble

    def isTrivial = (nominator % 10 == 0 && denominator % 10 == 0)
    def isDigitCancelling: Boolean = {
      def isEqualAfterCancellation(newNominator: Int, newDenominator: Int): Boolean = {
        math.abs((newNominator.toDouble / newDenominator.toDouble) - doubleValue) < epsilon
      }

      (nominator % 10 == denominator % 10 && isEqualAfterCancellation(nominator / 10, denominator / 10)) ||
      (nominator % 10 == denominator / 10 && isEqualAfterCancellation(nominator / 10, denominator % 10)) ||
      (nominator / 10 == denominator / 10 && isEqualAfterCancellation(nominator % 10, denominator % 10)) ||
      (nominator / 10 == denominator % 10 && isEqualAfterCancellation(nominator % 10, denominator / 10))
    }

    def simplify = {
      def gcd(x: Int, y: Int): Int =
        if (y == 0) x
        else gcd(y, x % y)

      def loop(n: Int, d: Int): (Int,Int) = {
        gcd(n, d) match {
          case 1 => (n, d)
          case gcd => loop(n / gcd, d / gcd)
        }
      }

      val (n,d) = loop(this.nominator, this.denominator)
      Problem33Fraction(n,d)
    }

    def multiply(that: Problem33Fraction) = {
      Problem33Fraction(that.nominator * nominator, that.denominator * denominator)
    }

    override def toString: String = {
      nominator.toString + "/" + denominator.toString
    }
  }

  def main(args: Array[String]) {
    val digitCancellingFractions =
      for {
        denominator <- 11 to 99
        nominator <- 10 until denominator
        fraction = Problem33Fraction(nominator, denominator)
        if (!fraction.isTrivial)
        if (fraction.isDigitCancelling)
      } yield {
        fraction
      }

    val product = (Problem33Fraction(1,1) /: digitCancellingFractions) {
      case (acc, value) => acc.multiply(value)
    }

    println {
      product.simplify.denominator
    }
  }
}
