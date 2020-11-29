import scala.math.abs

object fixpoint {
  val tolerance = 0.0001
  def isCloseEnough(a: Double, b: Double) = {
    abs((a - b) / b) < tolerance
  }

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iter(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iter(next)
    }
    iter(firstGuess)
  }

  def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1.0)
}