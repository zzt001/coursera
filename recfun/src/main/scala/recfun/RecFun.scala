package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(cnt: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) cnt == 0
      else if (cnt < 0) false
      else {
        loop({
          if (chars.head == '(') cnt + 1
          else if (chars.head == ')') cnt - 1
          else cnt
        }, chars.tail)
      }
    }
    loop(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, sortedCoins: List[Int]): Int = {
      if (sortedCoins.isEmpty || money < sortedCoins.head) {if (money == 0) 1 else 0}
      else {
        loop(money - sortedCoins.head, sortedCoins) + loop(money, sortedCoins.tail)
      }
    }
    loop(money, coins.sorted)
  }
}
