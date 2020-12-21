package exercises.advance

object PartialFunction extends App {
  val pf = new PartialFunction[Int, Int] {
    override def apply(v1: Int): Int = v1 match {
      case 1 => 144
      case 2 => 42
    }

    override def isDefinedAt(x: Int): Boolean = x == 1 || x == 2
  }
}
