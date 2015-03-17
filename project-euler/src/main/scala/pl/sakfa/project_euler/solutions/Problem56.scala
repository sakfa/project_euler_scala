package pl.sakfa.project_euler.solutions

object Problem56 extends App {
    println(((BigInt(1) until 100) flatMap { a =>
        (1 until 100) map { b =>
            (a pow b).toString.toCharArray.map(_ - 0x30).sum
        }
    }).max);
}