package lectures.part2oop

object MethodNotations extends App {
  class Person(val name: String, favouriteMovie: String, val age: Int) {
    def likes(movie: String): Boolean = movie == favouriteMovie
    def +(title: String): Person = new Person(s"${this.name} ($title)", favouriteMovie, age)
    def unary_+ : Person = new Person(name, favouriteMovie, age + 1)
    def learns(topic: String): String = s"${this.name} learns $topic"
    def learnsScala: String = learns("Scala")
    def apply(n: Int): String = s"${this.name} watched $favouriteMovie $n times"
  }
}
