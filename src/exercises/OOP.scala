package exercises

object OOP extends App {
  val writer = new Writer("John", "Sedgewick", 55)
  writer.fullname
}

class Writer(val firstName: String, val surname: String, val year: Int) {
  def fullname = s"${this.firstName} ${this.surname}"
}

