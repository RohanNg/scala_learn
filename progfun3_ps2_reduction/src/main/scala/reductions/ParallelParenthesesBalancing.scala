package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> false
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    chars(1) = '('
    chars(9999) = ')'
    chars(99999) = '('
    chars(999999) = ')'
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing extends App{

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    val length = chars.length;
    def balanceRec(pendingOpen: Int, pos: Int): Boolean =
      if (pos >= length)
        pendingOpen == 0
      else {
        if (pendingOpen < 0) false
        else chars(pos) match {
          case '(' => balanceRec(pendingOpen + 1, pos + 1)
          case ')' => balanceRec(pendingOpen - 1, pos + 1)
          case _ => balanceRec(pendingOpen, pos + 1)
        }
      }
    balanceRec(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    /**
      * ReduceToken._1 present number of ) char not having its ( counter part
      * ReduceToken._2 present number of ( char not having its ) counter part
      */
    type ReduceToken = (Int, Int)
    /**
      * Process array element in range [from, until)
      * produce the corresponding ReduceToken
      *
      * For example, given processed chars extracted from input range:
      *   (()     => (0,1)
      *   ))(((   => (2,3)
      *   ())(    => (1,1)
      */
    def generateToken(from: Int, until: Int) : ReduceToken = {
      var i = from
      // ) char not having its ( counter part
      var leftOpen = 0
      // ( char not having its ) counter part
      var rightOpen = 0
      while(i < until) {
        val char = chars(i)
        if(char == '(') rightOpen = rightOpen + 1
        else if (char == ')')
          if(rightOpen == 0) leftOpen = leftOpen + 1
          else rightOpen = rightOpen - 1
        i = i + 1
      }
      (leftOpen, rightOpen)
    }

    def reduce(left: ReduceToken, right: ReduceToken): ReduceToken = {
      val unClosed = left._2 - right._1
      if(unClosed <= 0) (left._1 - unClosed, right._2)
      else (left._1, right._2 + unClosed)
    }

    def parallelRunner(from: Int, until: Int): (Int, Int) = {
      if(until - from <= threshold) {
        generateToken(from, until)
      } else {
        val middle = (from + until)/2
        val (l,r) = parallel(parallelRunner(from, middle), parallelRunner(middle, until))
        reduce(l,r)
      }
    }
    parallelRunner(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!
  override def main(args: Array[String]): Unit = {
    println(parBalance("()()(()())()(())()()()()(()()(()())())()()".toCharArray,1))
  }
}
