package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def balanceCount(idx: Int, cnt: Int): Int = {
      if(cnt == -1) -1
      else if(idx == chars.length) cnt
      else if(chars(idx) == '(') balanceCount(idx + 1, cnt + 1)
      else if(chars(idx) == ')') balanceCount(idx + 1, cnt - 1)
      else balanceCount(idx + 1, cnt)
    }

    balanceCount(0, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      var left = 0
      var right = 0
      for(i <- idx until until) {
        if(chars(i) == '(') left = left + 1
        if(chars(i) == ')') {
          if(left > 1)  left = left - 1
          else right = right + 1
        }
      }
      (right, left)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if(until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val middle = from + (until-from)/2
        val (left, right) = parallel(reduce(from, middle), reduce(middle, until))

        //merge
        if(left._2 >= right._1) (left._1, left._2 - right._1 + right._2)
        else  (left._1 + right._1 - left._2, right._2)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
