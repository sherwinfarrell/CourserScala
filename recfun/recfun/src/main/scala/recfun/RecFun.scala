package recfun

import scala.math.Ordering.Float.TotalOrdering

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
      if(( c == 0) || ( c == r) ) 1

      else pascal(c, r-1) + pascal(c -1 , r -1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def checkParan(charsTail: List[Char], rem: Int): Boolean ={

      if(charsTail.isEmpty) {
        if(rem ==0)  true
        else  false
      }

      else if(charsTail.head =='(') {
        checkParan(charsTail.tail, rem + 1)

      }

      else if(charsTail.head== ')') {
          if(rem > 0) {
            checkParan(charsTail.tail, rem - 1)
          } else   false
        }

      else checkParan(charsTail.tail, rem)


    }
    if(!chars.contains('(') || !chars.contains(')') || chars.isEmpty)   false
    else if (checkParan(chars, 0) )  true
    else  false


  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {


        if(money == 0)   1
        else if(money < 0) 0
        else if (coins.isEmpty) 0

        else{
          countChange(money - coins.head , coins)+
            countChange(money, coins.tail)

        }

  }


}
