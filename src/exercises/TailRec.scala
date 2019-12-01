package exercises

import scala.annotation.tailrec

object TailRec extends App {
  def concat(str: String, times: Int) = {
    @tailrec
    def concatHelper(n: Int, strVal: String, accum: String): String =
      if (n <= 0) accum
      else concatHelper(n - 1, strVal, accum + strVal)
    concatHelper(times, str, "")
  }

  println(concat("String", 2))
  println(concat("String", 1))

  def fibonacci(n: Int): Int = {
    @tailrec
    def fibonacciHelper(i: Int, last: Int, nextToLast: Int): Int =
      if (i >= n) last
      else fibonacciHelper(i + 1, last + nextToLast, last)
    if (n <= 2) 1
    else fibonacciHelper(2, 1, 1)//1 1 2 3 5 8 13 21 34
  }

  println(fibonacci(9))
}
