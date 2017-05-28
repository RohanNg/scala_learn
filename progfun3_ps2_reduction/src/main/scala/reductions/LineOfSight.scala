package reductions

import java.util

import org.scalameter._
import common._

object LineOfSightRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight extends App {
  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    if (output.size < input.size) throw new IllegalArgumentException
    var i = 1
    var prev = 0f
    while (i < input.size) {
      prev = max(prev, input(i) / i)
      output(i) = prev
      i = i + 1
    }

  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
    override def toString: String = "{ " + left + ", " + right + " }"
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree {
    override def toString: String = "[" + from + " > " + until + " m=" + maxPrevious + "]"
  }

  /** Traverses the specified part of the array and returns the maximum angle.
    */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var i = from + 1
    var maxAngle = input(from) / from
    while (i < until) {
      maxAngle = max(maxAngle, input(i) / i)
      i = i + 1
    }
    maxAngle
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
    * returns the reduction tree for that part of the array.
    *
    * The reduction tree is a `Leaf` if the length of the specified part of the
    * array is smaller or equal to `threshold`, and a `Node` otherwise.
    * If the specified part of the array is longer than `threshold`, then the
    * work is divided and done recursively in parallel.
    */
  def upsweep(input: Array[Float], from: Int, end: Int, threshold: Int): Tree = {
    if (end - from <= threshold) Leaf(from, end, upsweepSequential(input, from, end))
    else {
      val middle = (end + from) / 2
      val (l, r) = parallel(upsweep(input, from, middle, threshold), upsweep(input, middle, end, threshold))
      Node(l, r)
    }
  }

  override def main(args: Array[String]): Unit = {
    val tree = upsweep(Array(1,2,3,4,10), 1, 5, threshold = 1)
    println(tree)
  }

  /** Traverses the part of the `input` array starting at `from` and until
    * `until`, and computes the maximum angle for each entry of the output array,
    * given the `startingAngle`.
    */
  def downsweepSequential(input: Array[Float], output: Array[Float],
                          startingAngle: Float, from: Int, until: Int): Unit = {
    var i = from;
    var currMax = startingAngle
    while (i < until) {
      currMax = max(currMax, input(i) / i)
      output(i) = currMax
      i = i + 1
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
    * reduction `tree` in parallel, and then calls `downsweepSequential` to write
    * the `output` angles.
    */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
                tree: Tree): Unit = tree match {
    case Leaf(from, until, maxPrevious) => downsweepSequential(input, output, max(maxPrevious, startingAngle), from, until)
    case Node(left, right) =>
      parallel(
        downsweep(input, output, startingAngle, left),
        downsweep(input, output, max(startingAngle, left.maxPrevious), right)
      )
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
                     threshold: Int): Unit = {
    downsweep(input, output, 0f, upsweep(input, 0, input.length, threshold))
  }
}