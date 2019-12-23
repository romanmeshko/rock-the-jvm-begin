package exercises.part3fp

object MapExercise extends App {
  val phonebook = Map(("Jim", 999), ("JIM", 5000))
  println(phonebook)
  println(phonebook.map(pair => pair._1.toLowerCase -> pair._2))

}
