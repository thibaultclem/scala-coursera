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
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def countBalance(remainingChars: List[Char], countOpenBracket: Int): Boolean = {
      //Check if still no ended parenthesis at the end of the string
      if (remainingChars.isEmpty && countOpenBracket <= 0) true
      //Check if ended bracket without a previous open parenthesis
      else if (remainingChars.isEmpty || (remainingChars.head == ')' && countOpenBracket <= 0)) false
      else countBalance(remainingChars.tail, updateCounter(remainingChars.head, countOpenBracket))
    }

    def updateCounter(char: Char, countOpenBracket: Int): Int =
      if (char == '(') countOpenBracket + 1 else if (char == ')') countOpenBracket - 1 else countOpenBracket

    countBalance(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeFirstInList(m: Int, cs: List[Int]): Int = {
      if (m == 0) 1
      else if (cs.isEmpty || m < 0) 0
      else countChangeFirstInList(m - cs.head, cs) + countChangeFirstInList(m, cs.tail)
    }

    if (coins.isEmpty || money == 0) 0
    else countChangeFirstInList(money, coins)
  }
}
