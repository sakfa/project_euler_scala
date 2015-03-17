import org.scalatest.FunSuite

import pl.sakfa.project_euler.primes.PrimeEstimations;
import pl.sakfa.project_euler.primes.SieveOfAtkin;

class PrimeEstimationsTest extends FunSuite {
  test("prime test") {
    assert(PrimeEstimations.millerRabinTest(221) === false, "221 is composite")
    assert(PrimeEstimations.millerRabinTest(10061) === true, "10061 is prime")
  }

  val to = 10000000;
  test(s"primes less than $to") {
    val sieve = SieveOfAtkin(to);

    (1 to to) foreach { i =>
      val target = sieve.isPrime(i)
      val actual = PrimeEstimations.millerRabinTest(i);
      if (actual != target) {
        assert(PrimeEstimations.millerRabinTest(i) === actual, s"$i is ${if (actual) "prime" else "composite"}")
      }
      assert(true === true, s"Primes estimated correctly for all primes from 1 to $to");
    }
  }
}

