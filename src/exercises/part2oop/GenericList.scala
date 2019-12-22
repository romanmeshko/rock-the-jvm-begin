package exercises.part2oop

abstract class GList[+A] {
  def head: A

  def tail: GList[A]

  def isEmpty: Boolean

  def add[B >: A](element: B): GList[B]

  override def toString: String = s"[$printElements]"

  def printElements: String

  def map[B](transformer: A => B): GList[B]

  def filter(predicate: A => Boolean): GList[A]

  def flatMap[B](transformer: A => GList[B]): GList[B]

  def ++[B >: A](list: GList[B]): GList[B]

  def foreach(action: A => Unit): Unit =
    if (isEmpty) ()
    else {
      action(head)
      tail.foreach(action)
    }

  def zipWith[B, C](list: GList[B], zipper: (A, B) => C): GList[C]

  def sort(compare: (A, A) => Int): GList[A]

  def fold[B](start: B)(func: (B, A) => B): B
}

object EmptyList extends GList[Nothing] {
  override def head: Nothing = throw new IllegalStateException

  override def tail: GList[Nothing] = throw new IllegalStateException

  override def isEmpty: Boolean = true

  override def add[B >: Nothing](element: B): GList[B] = new Cons(element, this)

  override def printElements: String = ""

  override def map[B](transformer: Nothing => B): GList[B] = this

  override def filter(predicate: Nothing => Boolean): GList[Nothing] = this

  override def flatMap[B](transformer: Nothing => GList[B]): GList[B] = this

  override def ++[B >: Nothing](list: GList[B]): GList[B] = list

  override def sort(compare: (Nothing, Nothing) => Int): GList[Nothing] = this

  override def zipWith[B, C](list: GList[B], zipper: (Nothing, B) => C): GList[C] =
    if (!list.isEmpty) throw new IllegalStateException
    else this

  override def fold[B](start: B)(func: (B, Nothing) => B): B = start
}

class Cons[+A](element: A, t: GList[A]) extends GList[A] {
  override def head: A = element

  override def tail: GList[A] = t

  override def isEmpty: Boolean = false

  override def add[B >: A](element: B): GList[B] = new Cons(element, this)

  override def printElements: String =
    if (t.isEmpty) s"$element"
    else s"$element ${t.printElements}"

  override def map[B](transformer: A => B): GList[B] =
    new Cons(transformer(element), t.map(transformer))

  override def filter(predicate: A => Boolean): GList[A] =
    if (predicate(element)) new Cons(element, t.filter(predicate))
    else t.filter(predicate)

  override def ++[B >: A](list: GList[B]): GList[B] = new Cons(element, t ++ list)

  override def flatMap[B](transformer: A => GList[B]): GList[B] =
    transformer(element) ++ t.flatMap(transformer)

  def sort(compare: (A, A) => Int): GList[A] = {
    def insert(a: A, sorted: GList[A]): GList[A] = {
      if (sorted.isEmpty) new Cons(a, EmptyList)
      else if (compare(a, sorted.head) <= 0) new Cons(a, sorted)
      else new Cons(sorted.head, insert(a, sorted.tail))
    }
    val sortedTail = t.sort(compare)
    insert(element, sortedTail)
  }

  override def zipWith[B, C](list: GList[B], zipper: (A, B) => C): GList[C] =
    new Cons[C](zipper(element, list.head), tail.zipWith(list.tail, zipper))

  override def fold[B](start: B)(func: (B, A) => B): B =
    tail.fold(func(start, head))(func)

}