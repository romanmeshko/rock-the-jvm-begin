package exercises.part2oop

object OOP extends App {
  val writer = new Writer("John", "Sedgewick", 55)
  writer.fullname
}

class Writer(val firstName: String, val surname: String, val year: Int) {
  def fullname = s"${this.firstName} ${this.surname}"
}

class Novel(val name: String, val releaseYear: Int, val writer: Writer) {
  def authorAge: Int = this.releaseYear - writer.year

  def isWrittenBy: Writer = writer

  def copy(newYearOfRelease: Int) = new Novel(this.name, newYearOfRelease, writer)
}

class Counter(val value: Int) {
  def currentCount: Int = this.value

  def inc(delta: Int): Counter = new Counter(this.value + delta)

  def dec(delta: Int): Counter = new Counter(this.value - delta)

  def inc: Counter = inc(1)

  def dec: Counter = dec(1)
}