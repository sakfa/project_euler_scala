package pl.sakfa.project_euler.solutions

object Problem55 extends App {
    def isPalindrome(n: BigInt) = n.toString == n.toString.reverse

    val iterationLimit = 50;
    def isLychrel(n: BigInt, iterationsLeft: Int = 50, tail: List[Int] = Nil): Boolean = {
        if (iterationsLeft == 0) true
        else {
            val nn = n + BigInt(n.toString.reverse);
            if (isPalindrome(nn)) false
            else isLychrel(nn, iterationsLeft - 1)
        }
    }

    println (
        (1 until 10000).filter(isLychrel(_)).size
    );
}