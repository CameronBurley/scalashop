package scalashop

import java.util.concurrent.*
import scala.util.DynamicVariable
import org.scalameter.*

import scala.language.postfixOps

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
def rgba(r: Int, g: Int, b: Int, a: Int): RGBA =
  (r << 24) | (g << 16) | (b << 8) | (a << 0)

/** Restricts the integer into the specified range. */
def clamp(v: Int, min: Int, max: Int): Int =
  if v < min then min
  else if v > max then max
  else v

/** Image is a two-dimensional matrix of pixel values. */
class Img(val width: Int, val height: Int, private val data: Array[RGBA]):
  def this(w: Int, h: Int) = this(w, h, new Array(w * h))
  def apply(x: Int, y: Int): RGBA = data(y * width + x)
  def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c

/** Computes the blurred RGBA value of a single pixel of the input image. */
def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA =
  def computeAvg(total: Int, count: Int): Int = if (count == 0) 0 else total / count

  // TODO implement using while loops
  var redTotal = 0
  var greenTotal = 0
  var blueTotal = 0
  var alphaTotal = 0
  var count = 0

  // Set boundary indices
  val xStart = clamp(x - radius, 0, src.width - 1)
  val xEnd = clamp(x + radius, xStart, src.width - 1)
  val yStart = clamp(y - radius, 0, src.height - 1)
  val yEnd = clamp(y + radius, yStart, src.height - 1)

  var xi = xStart
  while (xi <= xEnd) {
    var yi = yStart
    while (yi <= yEnd) {
      val pixel = src(xi, yi)
      redTotal += red(pixel)
      greenTotal += green(pixel)
      blueTotal += blue(pixel)
      alphaTotal += alpha(pixel)
      count += 1
      yi += 1
    }
    xi += 1
  }

  rgba(computeAvg(redTotal, count), computeAvg(greenTotal, count), computeAvg(blueTotal, count), computeAvg(alphaTotal, count))

val forkJoinPool = ForkJoinPool()

abstract class TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T]
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())

class DefaultTaskScheduler extends TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T] =
    val t = new RecursiveTask[T] {
      def compute = body
    }
    Thread.currentThread match
      case wt: ForkJoinWorkerThread =>
        t.fork()
      case _ =>
        forkJoinPool.execute(t)
    t

val scheduler =
  DynamicVariable[TaskScheduler](DefaultTaskScheduler())

def task[T](body: => T): ForkJoinTask[T] =
  scheduler.value.schedule(body)

def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
  scheduler.value.parallel(taskA, taskB)

def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) =
  val ta = task { taskA }
  val tb = task { taskB }
  val tc = task { taskC }
  val td = taskD
  (ta.join(), tb.join(), tc.join(), td)
