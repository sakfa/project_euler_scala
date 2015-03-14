package pl.sakfa.project_euler.solutions;

import math.BigInt

object Problem20 {
  def main(args: Array[String]) {
    val factorial = (2 to 100).map(BigInt(_)).product;
    println(factorial.toString.toCharArray.map(_.toInt - '0'.toInt).sum);
  }
}
