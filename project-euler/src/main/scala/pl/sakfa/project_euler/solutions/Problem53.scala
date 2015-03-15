package pl.sakfa.project_euler.solutions

import scala.math.BigInt;

object Problem53 extends App {

    def pascal(rows: Int): List[List[BigInt]] = {
        if (rows == 0) List(BigInt(1)) :: Nil;
        else if (rows == 1) List(BigInt(1), BigInt(1)) :: pascal(0);
        else {
            val previousTriangle = pascal(rows - 1);
            val nextRow = BigInt(1) :: previousTriangle.head.sliding(2).map(
                (window: List[BigInt]) => window.head + window.tail.head
            ).toList ::: (BigInt(1) :: Nil);
            nextRow :: previousTriangle;
        }
    }

    println {
        pascal(53).map(row =>
            row.filter(_ > BigInt(1000000)).size
        ).sum
    }
}