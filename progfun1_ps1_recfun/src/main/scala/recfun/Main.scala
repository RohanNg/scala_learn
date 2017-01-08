package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    *
    * @param c
    * @param r
    * @return
    */
  def pascal(c: Int, r: Int): Int = {
    if (r == c) 1
    else if (c == 0) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /** Check a list of character for parentheses balance
    * A list have is parentheses balanced if for every
    * opening parenthesis a corresponding closing parent
    * thesis can be found, and vice versa.
    * Example of list with balanced parenthesis
    *   "(()())".toList
    *   "()()()".toList
    * Example of unbalanced
    *   ")()".toList
    *   "()())".toList
    * @param chars list of character to be checked
    * @return true if the list of chars is parentheses balanced
    */
  def balance(chars: List[Char]): Boolean = {
    /** Idea: this function provide logic to keep track of the
      * number of closing parenthesis to be expected from the
      * remaining of the list (be cause the discovered opening
      * parenthesis must have its mate)
      */
    @tailrec
    def loop(chars: List[Char], closePar: Int): Boolean = {
      if (closePar < 0) false
      else if (chars.isEmpty) if (closePar == 0) true else false
      else {
        val char = chars.head
        if (char == '(') loop(chars.tail, closePar + 1)
        else if (char == ')') loop(chars.tail, closePar - 1)
        else loop(chars.tail, closePar)
      }
    }

    loop(chars, 0)
  }


  /** Count how many way an amount of money can be exchanged
    * for given a set of available money denominations
    *
    * For example, countChange(4,List(1,2)) = 3
    * because 4 = 1 + 1 + 1 + 1 = 1 + 1 + 2 = 2 + 2
    * as there are there possible way to change, or break dow 4
    *
    * @param money the amount of money
    * @param coins list of unique money denominations
    * @return number of ways an amount of money can be exchanged
    * for given a set of available money denominations
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def acc(money: Int, coin: Int, coins: List[Int], accu: Int): Int = {
      if (money < 0) accu
      else {
        //      println("loop(" + money + ", " + coins +" ) = " + loop(money,coins))
        acc(money - coin, coin, coins, accu + loop(money, coins))
      }
    }

    def loop(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (coins.isEmpty) 0
      else {
        val head = coins.head
        val tail = coins.tail
        // an optimization
        // if money can not be break down further
        // (because every denominations remaining in the tail is bigger than the amount)
        // and the current denomination (head) is enough to account for the money
        if (!tail.isEmpty && money < tail.head && money % head == 0) 1
        else acc(money, head, tail, 0)
      }
    }

    loop(money, coins.sortWith(_ < _))
  }
  // the above function is an impletation of the following IDEA, or ALGORITHMS
  // amount 4  list (1,2)
  // peak 1:
  //    4 (2)   = 0 + ?
  //   peak 2:
  //      4 (0) => no way to get 4 from empty list
  //      2 (0) => no way
  //      0 (0) = 2 + 2
  //    3 (2)   = 1 + ?
  //   peak 2:
  //      3 (0) => no way
  //      2 (0) => no way
  //      1 (0) => no way
  //    2 (2)   = 1 + 1 + ?
  //   peak 2:
  //      2 (0) => no way
  //      0 (0) => 2 + 2
  //    1 (2)   = 1 + 1 + 1 + ?
  //      1 < 2 => no way
  //    0 (2)   = 1 + 1 + 1 + 1
}
