package exercises.part2oop

abstract class MyList {
  def head: Int
  def tail: MyList
  def isEmpty: Boolean
  def add(element: Int): MyList
  override def toString: String = s"[ $printElements ]"
  def printElements: String
}

object EmptyList extends MyList {
  override def head: Int = throw new IllegalStateException
  override def tail: MyList = throw new IllegalStateException
  override def isEmpty: Boolean = true
  override def add(element: Int): MyList = new Cons(element, this)
  override def printElements: String = ""
}

class Cons(element: Int, t: MyList) extends MyList {
  override def head: Int = element
  override def tail: MyList = t
  override def isEmpty: Boolean = false
  override def add(element: Int): MyList = new Cons(element, this)
  override def printElements: String =
    if (t.isEmpty) s"$element"
    else s"$element ${t.printElements}"
}
