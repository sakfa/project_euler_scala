package pl.sakfa.project_euler.solutions;

import math.BigInt

object Problem17 {
  val ones = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine");
  val teens = List("eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen");
  val tens = List("ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety");

  def spellNumber(n: Int): String = {
    if (n >= 1 && n <= 9) ones(n - 1);
    else if (n >= 11 && n <= 19) teens(n - 11);
    else if (n < 100) {
        val d = n / 10;
        val r = n % 10;
        tens(d - 1) + (if (r > 0) "-" + spellNumber(r) else "");
    }
    else if (n == 1000) "one thousand";
    else if (n % 100 == 0) {
        val d = n / 100
        spellNumber(d) + " hundred"
    }
    else {
        val d = n / 100
        val r = n % 100
        spellNumber(d * 100) + " and " + spellNumber(r)
    }
  }

  def main(args: Array[String]) {
    var sum = (1 to 1000)
        .map(n => spellNumber(n))
        .map(s => s.replaceAll("[^a-zA-Z]", ""))
        .map(s => s.length)
        .sum;
    println(sum);
  }
}
