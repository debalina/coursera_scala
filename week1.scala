package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println() 
      } 
      
      println("Balance")
      if (balance("(just) an example".toList)) println("True")
      else println("False")
      if (balance(":-)".toList)) println("True")
      else println("False")
      if (balance("())(".toList)) println("True")
      else println("False")
      if (balance("I told him (that it�s not (yet) done). (But he wasn�t listening)".toList)) println("True")
      else println("False")
      
      if (balance("(if (zero? x) max (/ 1 x))".toList)) println("True")
      else println("False")
      
      println("CountChange")
      println(countChange(5,List(1,2)))
      println(countChange(4,List(1,2,3)))
      println(countChange(6,List(1,2,3)))
  }
  

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if ((c == 0) || (c == r)) 1 else pascal(c - 1, r -1) + pascal (c, r- 1)
   
  /**
   * Exercise 2
   * This exercise counts the matching parenthesis in an expression.
   */
   def balance(chars: List[Char]): Boolean = {
      def countMatching(acc: Int, left:Int, right:Int, chars:List[Char]): Boolean = {
       if ((chars.isEmpty) && (acc == 0) && (left == right)) true
       else if (chars.isEmpty) false
       else if (chars.head == '(') countMatching(acc+1, left+1, right, chars.tail)
       else if ((chars.head == ')') && (acc > 0)) countMatching(acc-1, left, right + 1, chars.tail)
       else if (chars.head == ')') countMatching(acc, left, right + 1, chars.tail)
       else countMatching(acc, left, right, chars.tail)
      }
       countMatching(0,0, 0, chars)
    } 
    
     
        

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    /* if money becomes 0, there is a valid way of counting change
     * but if the list of coins become empty or money becomes less
     * than zero, then that particular combination does not lead to 
     * success. Otherwise, we use the recursive formula.
     */
    if (money == 0) 1
    else if ((coins.isEmpty) || (money < 0)) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
