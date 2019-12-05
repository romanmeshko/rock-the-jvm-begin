package exercises.part2oop

abstract class GList[+A] {
  def head: A
  def tail: GList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): GList[B]
  override def toString: String = s"[ $printElements ]"
  def printElements: String
}

object EmptyList extends GList[Nothing] {
  override def head: Nothing = throw new IllegalStateException
  override def tail: GList[Nothing] = throw new IllegalStateException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](element: B): GList[B] = new Cons(element, this)
  override def printElements: String = ""
}

class Cons[+A](element: A, t: GList[A]) extends GList[A] {
  override def head: A = element
  override def tail: GList[A] = t
  override def isEmpty: Boolean = false
  override def add[B >: A](element: B): GList[B] = new Cons(element, this)
  override def printElements: String =
    if (t.isEmpty) s"$element"
    else s"$element ${t.printElements}"
}
