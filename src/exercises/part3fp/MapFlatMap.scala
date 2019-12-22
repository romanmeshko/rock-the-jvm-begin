package exercises.part3fp

import exercises.part2oop.{EmptyList, Cons}

object MapFlatMap extends App {
  val numbers = List(1, 2, 3, 4)
  val chars = List("a", "b" , "c", "d")

  numbers.flatMap(n => chars.map(c => c + n.toString))

  val intList = new Cons(1, new Cons( 2, new Cons(3 , EmptyList)))
  val charList = new Cons("a", new Cons("b", new Cons("c", EmptyList)))

  val comb = intList.flatMap(i => charList.map(c => c + i.toString))

  val combFor = for {
    i <- intList
    c <- charList
  } yield c + i.toString

  println(combFor)


}
