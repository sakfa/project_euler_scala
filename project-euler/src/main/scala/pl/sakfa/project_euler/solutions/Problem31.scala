package pl.sakfa.project_euler.solutions

import pl.sakfa.project_euler.tools.TimeTool

/*
In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1£1 + 150p + 220p + 15p + 12p + 31p
How many different ways can £2 be made using any number of coins?
 */
object Problem31 {
  def countChange(money: Int, coins: List[Int]): Int = {
    require(money >= 0, "Money must not be negative.")

    def countCombinations(left: Int, maxCoinGivenSoFar: Int): Int = {
      if (left < 0) 0 			//this combination did not yield correct change
      else if (left == 0) 1		//this did
      else {
        //lets filter out coins smaller than current biggest given to avoid permutations of combinations
        coins.filterNot(_ < maxCoinGivenSoFar).map { coin =>
          countCombinations(left - coin, if (coin > maxCoinGivenSoFar) coin else maxCoinGivenSoFar)
        }.sum
      }
    }

    countCombinations(money, 0)
  }

  def main(args: Array[String]) {
    TimeTool.start()
    println(countChange(200, List(1, 2, 5, 10, 20, 50, 100, 200)))
    TimeTool.printAndStop()
  }
}
