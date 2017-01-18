
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val center = src(x,y)
    var sum: (Long, Long, Long, Long) = (0,0,0,0)
    val neighborInRadius = math.pow(radius * 2 + 1, 2)
    for {
      yy <- (y - radius) to (y + radius)
      xx <- (x - radius) to (x + radius)
    } {
      println(s"x: ${clamp(xx, 0, src.width)}, y: ${ clamp(yy, 0, src.height)}, img : ${src.toString}")
      val currentPixel = src(clamp(xx, 0, src.width), clamp(yy, 0, src.height))
      sum = (sum._1 + red(currentPixel), sum._2 + green(currentPixel), sum._3 + blue(currentPixel), sum._4 + alpha(currentPixel))
    }

    rgba((sum._1/neighborInRadius).toInt, (sum._2/neighborInRadius).toInt, (sum._3/neighborInRadius).toInt, (sum._4/neighborInRadius).toInt)
  }
}

object test extends App {
  import scalashop._
  val data = Array(1,1,1,1,1,1,1,1,1)
  val img = new Img(2,2, data)
  println(boxBlurKernel(img, 0,0, 2))
}