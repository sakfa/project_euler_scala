package pl.sakfa.project_euler.solutions

object Problem59 extends App {
    val chars = io.Source.fromInputStream(getClass.getResourceAsStream("/problem59_cipher.txt"))
        .getLines.next
        .split(",")
        .map(_.toByte);

    lazy val charsCount = chars.size

    def doCipher(chars: Array[Byte], key: Array[Byte]): Array[Byte] = {
        lazy val keyStream: Stream[Byte] = key.toStream #::: keyStream;

        (chars zip keyStream) map { case (char: Byte, cipher: Byte) =>
            (char ^ cipher).toByte
        }
    }
    def letterFrequencies(chars: Array[Byte]) = {
        chars
            .filter(b => (b >= 'a'.toByte && b <= 'z'.toByte) || (b >= 'A'.toByte && b <= 'Z'.toByte))
            .map(b => if (b < 'a') (b - 'A'.toByte + 'a'.toByte).toByte else b)
            .groupBy(b => b)
            .mapValues(_.size.toDouble / charsCount)
    }

    def isInteresting(freq: Map[Byte, Double]) = {
        val frequent = Map(
            'a' -> 8.167, 'e' -> 12.702, 'i' -> 6.966, 'o' -> 7.507, 't' -> 9.056
        )
        val rare = List('z', 'x', 'q', 'j')
        val frequentTolerance = 2.0 //accept frequent if it is 2 times less frequent than average
        val rareThreshold = 0.003 //accept rare even if twice as frequent as average

        (frequent.forall { case (letter, frequency) =>
            freq.getOrElse(letter.toByte, 0.0) > frequency/(100*frequentTolerance)
        }) &&
        rare.forall(c => freq.getOrElse(c.toByte, 0.0) < rareThreshold)
    }

    var letter = ('a' to 'z')
    val keys =
    letter flatMap { a =>
        letter flatMap { b =>
            letter map { c =>
                Array(a.toByte, b.toByte, c.toByte)
            }
        }
    }

    val interesting = keys.filter(k => isInteresting(letterFrequencies(doCipher(chars, k))))
        .foreach { k =>
            println(s"INTERESTING KEY ${k.map(_.toChar).mkString("")}");
            val text = doCipher(chars, k);
            text.map(_.toChar).foreach(print);
            println
            println(s"Sum of chars: ${text.foldLeft(0)(_ + _)}")
        }
}
