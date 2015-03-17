package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.primes.SieveOfAtkin

object Problem60 extends App {
    var primes: List[Int] = Nil
    var pairs: List[(Int, Int)] = Nil
    var triples: List[(Int, Int, Int)] = Nil
    var fours: List[(Int, Int, Int, Int)] = Nil
    var fives: List[(Int, Int, Int, Int, Int)] = Nil

    val sieve = SieveOfAtkin(100000000)

    def isPrimePair(a: Int, b: Int) = {
        sieve.isPrime((a.toString + b.toString).toInt) &&
        sieve.isPrime((b.toString + a.toString).toInt)
    }
    def isPrimeTriple(a: Int, b: Int, c: Int) = {
        isPrimePair(a,b) && isPrimePair(a,c) && isPrimePair(b,c);
    }
    def isPrimeFour(a:Int, b: Int, c: Int, d: Int) = {
        isPrimePair(a,b) && isPrimePair(a,c) && isPrimePair(a,d) &&
        isPrimePair(b,c) && isPrimePair(b,d) && isPrimePair(c,d);
    }
    def isPrimeFive(a:Int,b:Int,c:Int,d:Int,e:Int) = {
        isPrimePair(a,b) && isPrimePair(a,c) && isPrimePair(a,d) && isPrimePair(a,e) && isPrimePair(b,c) &&
        isPrimePair(b,d) && isPrimePair(b,e) && isPrimePair(c,d) && isPrimePair(c,e) && isPrimePair(d,e)
    }

    Stream.from(2)
        .filter(sieve.isPrime(_))
        .map { i =>
            primes = i :: primes;
            pairs = primes.filter(p => isPrimePair(i,p)).map(p =>(i,p)) ::: pairs;
            triples = pairs.filter(p => isPrimeTriple(i,p._1,p._2)).map(p => (i,p._1,p._2)) ::: triples
            fours = triples.filter(p => isPrimeFour(i,p._1,p._2,p._3)).map(p => (i,p._1,p._2,p._3)) ::: fours
            fives = fours.filter(p => isPrimeFive(i,p._1,p._2,p._3,p._4)).map(p => (i,p._1,p._2,p._3,p._4)) ::: fives
            fives.size
        }.takeWhile(_ == 0).toList;

    val five = fives.head
    val sum = five._1 + five._2 + five._3 + five._4 + five._5
    println(s"Fives: $five, sum: $sum");
}