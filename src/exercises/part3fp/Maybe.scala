package exercises.part3fp

abstract class Maybe[+T] {
  def filter(p: T => Boolean): Maybe[T]

  def map[U](f: T => U): Maybe[U]

  def flatMap[U](f: T => Maybe[U]): Maybe[U]
}

case object None extends Maybe[Nothing] {
  override def filter(p: Nothing => Boolean): Maybe[Nothing] = this

  override def map[U](f: Nothing => U): Maybe[U] = this

  override def flatMap[U](f: Nothing => Maybe[U]): Maybe[U] = this
}

case class Just[T](value: T) extends Maybe[T] {
  override def filter(p: T => Boolean): Maybe[T] =
    if (p(value)) this
    else None

  override def map[U](f: T => U): Maybe[U] = Just(f(value))

  override def flatMap[U](f: T => Maybe[U]): Maybe[U] = f(value)
}
