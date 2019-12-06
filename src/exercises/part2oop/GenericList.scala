package exercises.part2oop

abstract class GList[+A] {
  def head: A
  def tail: GList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): GList[B]
  override def toString: String = s"[ $printElements ]"
  def printElements: String
  def map[B](transformer: MyTransformer[A, B]): GList[B]
  def filter(predicate: MyPredicate[A]): GList[A]
  def flatMap[B](transformer: MyTransformer[A, GList[B]]): GList[B]
  def ++[B >: A](list: GList[B]): GList[B]
}

object EmptyList extends GList[Nothing] {
  override def head: Nothing = throw new IllegalStateException
  override def tail: GList[Nothing] = throw new IllegalStateException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](element: B): GList[B] = new Cons(element, this)
  override def printElements: String = ""
  override def map[B](transformer: MyTransformer[Nothing, B]): GList[B] = this
  override def filter(predicate: MyPredicate[Nothing]): GList[Nothing] = this
  override def flatMap[B](transformer: MyTransformer[Nothing, GList[B]]): GList[B] = this
  override def ++[B >: Nothing](list: GList[B]): GList[B] = list
}

class Cons[+A](element: A, t: GList[A]) extends GList[A] {
  override def head: A = element
  override def tail: GList[A] = t
  override def isEmpty: Boolean = false
  override def add[B >: A](element: B): GList[B] = new Cons(element, this)
  override def printElements: String =
    if (t.isEmpty) s"$element"
    else s"$element ${t.printElements}"
  override def map[B](transformer: MyTransformer[A, B]): GList[B] =
    new Cons(transformer.transform(element), t.map(transformer))
  override def filter(predicate: MyPredicate[A]): GList[A] =
    if (predicate.test(element)) new Cons(element, t.filter(predicate))
    else t.filter(predicate)
  override def ++[B >: A](list: GList[B]): GList[B] = new Cons[B](element, t ++ list)
  override def flatMap[B](transformer: MyTransformer[A, GList[B]]): GList[B] =
    transformer.transform(element) ++ t.flatMap(transformer)
}

trait MyPredicate[-T] {
  def test(t: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(a: A): B
}
