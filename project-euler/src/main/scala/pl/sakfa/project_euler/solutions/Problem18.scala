package pl.sakfa.project_euler.solutions;

import math.max;

object Problem18 extends App {
    val triangleInput = """
        |75
        |95 64
        |17 47 82
        |18 35 87 10
        |20 04 82 47 65
        |19 01 23 75 03 34
        |88 02 77 73 07 63 67
        |99 65 04 28 06 16 70 92
        |41 41 26 56 83 40 80 70 33
        |41 48 72 33 47 32 37 16 94 29
        |53 71 44 65 25 43 91 52 97 51 14
        |70 11 33 28 77 73 17 78 39 68 17 57
        |91 71 52 38 17 14 91 43 58 50 27 29 48
        |63 66 04 68 89 53 67 30 73 16 69 87 40 31
        |04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
    """.stripMargin.trim;

    val triangle = triangleInput.split("\n").map(s =>
        s.split(" ")
         .map(_.toInt)
    );

    def nextRow(currentIdx: Int, current: Array[BigInt]): Array[BigInt] = {
        if (currentIdx == 0) triangle(0).map(BigInt(_));
        else {
            val row = new Array[BigInt](currentIdx + 1);
            (0 to (currentIdx)) foreach { i =>
                val left = if (i > 0) current(i - 1) else BigInt(-1);
                val right = if (i < currentIdx) current(i) else BigInt(-1);

                row(i) = BigInt(triangle(currentIdx)(i)) + (if (left > right) left else right);
            }

            row
        }
    }

    def nthRow(n: Int): Array[BigInt] = {
        var current: Array[BigInt] = nextRow(0, Array(BigInt(0)));
        (1 to n) foreach { i =>
            current = nextRow(i, current);
        }
        current
    }

    println ( nthRow(triangle.size - 1).max );
}