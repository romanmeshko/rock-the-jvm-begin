package exercises.part3fp

object FunctionExc {
  val concat: (String, String) => String = new Function2[String, String, String] {
    override def apply(str1: String, str2: String): String = str1 + str2
  }

  val specExcFunction = new Function1[Int, Function1[Int, Int]] {
    override def apply(i: Int): Int => Int = new Function1[Int, Int] {
      override def apply(inner: Int): Int = i + inner
    }
  }

  val funcFunc: Int => Int => Int = x => y => x + y
}
