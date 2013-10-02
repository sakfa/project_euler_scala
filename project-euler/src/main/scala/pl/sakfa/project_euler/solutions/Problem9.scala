package pl.sakfa.project_euler.solutions


object Problem9 {
  def sqr(n: Int) = n * n

  def main(args: Array[String]){
    for (
      a <- 1 to 1000; b <- a + 1 to 1000
      if sqr(a) + sqr(b) == sqr(1000 - a - b)
    ) {
      val c = (1000 - a - b)
      println("a = " + a + ", b = " + b + ", c = " + c)
      println("abc = " + a * b * c)
      return
    }
  }
}
