package scalashop

import org.scalameter._
import common._

import scala.annotation.tailrec

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
    * `dst`, starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each column, `blur` traverses the pixels by going from top to
    * bottom.
    */
  @tailrec
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit =
    if (from >= end) ()
    else {
      for (h <- 0 until src.height) {
        dst(from, h) = boxBlurKernel(src, from, h, radius)
      }
      blur(src, dst, from + 1, end, radius)
    }


  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * columns.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {

    val div = src.width / numTasks

    val steps = (0 to src.width).by(if (div == 0) 1 else div).toList.take(numTasks)
    val strips = steps.zip(steps.tail ::: List(src.width))

    val finalStrip = if (strips.length == numTasks) strips
    else strips ++ List.fill(numTasks - strips.length)((src.width, src.width))

    assert(numTasks == finalStrip.length, "number of task exercised in parallel should equal " + numTasks)

    def loop(strips: List[(Int, Int)]): Unit = strips match {
      case Nil => ()
      case x :: xs => {
        val computation = task {
          blur(src, dst, x._1, x._2, radius)
        }
        loop(xs)
        computation.join()
      }
    }

    loop(finalStrip)
  }

}
