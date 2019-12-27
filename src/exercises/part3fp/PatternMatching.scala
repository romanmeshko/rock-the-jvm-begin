package exercises.part3fp

object PatternMatching extends App {
  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr

  def mkStr(expr: Expr): String = expr match {
    case Number(value) => value.toString
    case Sum(e1, e2) => mkStr(e1) + " + " + mkStr(e2)
    case Prod(e1: Sum, e2) => "(" + mkStr(e1) + ") * " + mkStr(e2)
    case Prod(e1, e2: Sum) => mkStr(e1) + " * (" + mkStr(e2) + " )"
    case Prod(e1, e2) => mkStr(e1) + " * " + mkStr(e2)
  }

  println(mkStr(Sum(Number(2), Number(3))))
  println(mkStr(Sum(Sum(Number(2), Number(3)), Number(4))))
  println(mkStr(Prod(Sum(Number(1), Number(2)), Number(3))))
  Prod(Sum(Number(1), Number(2)), Sum(Number(3), Number(7)))
  println(mkStr(Sum(Prod(Number(2), Number(3)), Number(4))))
}
