package exercises.part1basics

object Functions extends App {
  def greeting(name: String, age: Int): String =
    "Hi, my name is " + name + " and I am " + age + " years old"

  def factorial(i: Long) = {
    def factRec(j: Long, result: Long): Long = {
      if (j == 1) result
      else factRec(j - 1, result * j)
    }
    factRec(i, 1)
  }

  def fibonacci(i: Long): Long = {
    if (i == 1) 1
    else if (i == 2) 1
    else fibonacci(i - 1) + fibonacci(i - 2)
  }

  println(fibonacci(5))
}
