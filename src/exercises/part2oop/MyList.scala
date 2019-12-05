package exercises.part2oop

abstract class MyList {
  def head: Int
  def tail: GList
  def isEmpty: Boolean
  def add(element: Int): GList
  override def toString: String = s"[ $printElements ]"
  def printElements: String
}

object EmptyList extends GList {
  override def head: Int = throw new IllegalStateException
  override def tail: GList = throw new IllegalStateException
  override def isEmpty: Boolean = true
  override def add(element: Int): GList = new Cons(element, this)
  override def printElements: String = ""
}

class Cons(element: Int, t: GList) extends GList {
  override def head: Int = element
  override def tail: GList = t
  override def isEmpty: Boolean = false
  override def add(element: Int): GList = new Cons(element, this)
  override def printElements: String =
    if (t.isEmpty) s"$element"
    else s"$element ${t.printElements}"
}
