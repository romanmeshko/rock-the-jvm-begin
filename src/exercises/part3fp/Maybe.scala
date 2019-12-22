package exercises.part3fp

class Maybe[+T](element: T) {
  def filter(pred: T => Boolean): T =
    if (element == null) null
    else if (pred(element)) element
    else null
  def map[U](f: T => U): U
}

object None extends Maybe[Nothing]
