import org.scalatest.FunSuite
import pl.sakfa.project_euler.solutions.Problem51

class Problem51Test extends FunSuite {
  test("index of digit") {
    assert(Problem51.indicesOfDigit("123", '1') === List(0), "123/1")
    assert(Problem51.indicesOfDigit("123", '4') === Nil, "123/4")
    assert(Problem51.indicesOfDigit("1231", '1') === List(0, 3), "1231/1")
  }

  test("findSameDigitsCombinations for one digit number") {
    assert(Problem51.findSameDigitsCombinations(5) === List(List(0)))
  }

  test("findSameDigitsCombinations for same digits number") {
    assert(Problem51.findSameDigitsCombinations(55).toSet === Set(List(0), List(1), List(0, 1)))
  }

  test("findSameDigitsCombinations for longer number") {
    assert(Problem51.findSameDigitsCombinations(515).toSet === Set(List(0), List(2), List(0, 2), List(1)))
  }

  test("replace digits") {
    assert(Problem51.replaceDigits(31233, List(0, 3), 4) === 41243)
  }

  test("primeNumberFamilySize") {
    assert(Problem51.primeNumberFamily(13, List(0)) === List(13, 23, 43, 53, 73, 83))
    assert(Problem51.primeNumberFamily(13, List(1)) === List(11, 13, 17, 19))
    assert(Problem51.primeNumberFamily(13, List(0, 1)) === List(11))
  }

  test("max") {
    assert(Problem51.maxPrimeValueFamilyLength(13) === 6)
  }
}

