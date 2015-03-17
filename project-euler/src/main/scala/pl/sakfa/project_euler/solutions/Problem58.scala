package pl.sakfa.project_euler.solutions

import scala.math.log;
import pl.sakfa.project_euler.primes.PrimeEstimations;

object Problem58 extends App {
    //a bit of stream magic: this Stream will generate infinite stream of numbers that can be found on diagonal of spiral (excluding central '1')
    val diagonalStreamWithCounter: Stream[(Int,Int,Int)] = (3,2,1) #:: diagonalStreamWithCounter.map { case (cell, increment, cornerCounter) =>
        (cell + increment, if (cornerCounter == 3) increment + 2 else increment, (cornerCounter + 1) % 4)
    };

    //and this will generate numbers on diagonals together with it's prime check result
    val diagonalStream = diagonalStreamWithCounter.map(_._1);

    //so now for algorithm:
    var primes = 0;
    var numbers = 1;

    val hd = (diagonalStream.sliding(4, 4) map { ns: Stream[Int] =>
        numbers += 4;
        primes += ns.count(PrimeEstimations.millerRabinTest(_));
        (primes, numbers)
    }).filter( (tpl: (Int, Int)) => tpl._1.toDouble / tpl._2 < 0.10 ).next;

    println(s"$hd._1 primes / $hd._2 numbers at side = ${(hd._2 + 1) / 2}")
}