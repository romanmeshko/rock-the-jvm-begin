package exercises.advance

trait MySet[A] extends (A => Boolean) {
  def contains(elem: A): Boolean = this(elem)
  def +(elem: A): MySet[A] = this + elem
  def ++(another: MySet[A]): MySet[A] = this ++ another

  def map[B](f: A => B): MySet[B] = this.map(f)
  def flatMap[B](f: A => MySet[B]): MySet[B] = this.flatMap(f)
  def filter(predicate: A => Boolean): MySet[A] = this.filter(predicate)
  def foreach(f: A => Unit): Unit = this.foreach(f)
}
