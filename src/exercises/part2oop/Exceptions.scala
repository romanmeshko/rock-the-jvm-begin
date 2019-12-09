package exercises.part2oop

object Exceptions extends App {
  def oomError: Unit = {
    val i = new Array[Int](Int.MaxValue)
  }

  def soError: Int = {
    soError * soError
  }

  println(PocketCalculator.+(3, 4))

  object PocketCalculator {
    def +(x: Int, y: Int): Int = {
      val result = x + y
      if (x > 0 && y > 0 && result < 0) throw new OverflowException
      else if (x < 0 && y < 0 && result > 0) throw new UnderflowException
      else result
    }

    def -(x: Int, y: Int): Int = {
      val result = x - y
      if (x > 0 && y < 0 && result < 0) throw new OverflowException
      else if (x < 0 && y > 0 && result > 0) throw new UnderflowException
      else result
    }

    def *(x: Int, y: Int): Int = {
      val result = x * y
      if (x > 0 && y > 0 && result < 0) throw new OverflowException
      else if (x < 0 && y < 0 && result < 0) throw new OverflowException
      else if (x > 0 && y < 0 && result > 0) throw new UnderflowException
      else if (x < 0 && y > 0 && result > 0) throw new UnderflowException
      else result
    }

    def /(x: Int, y: Int): Int =
      if (y == 0) throw new MathCalculationException
      else x / y
  }

  class OverflowException extends Exception
  class UnderflowException extends Exception
  class MathCalculationException extends Exception("Division by 0")
}
