package exercises.part3fp

import scala.util.{Random, Try}

object HandleFail extends App {
  def testFail = throw new IllegalArgumentException

  val aTry = Try(testFail)

  val hostName = "localhost"
  val port = "8080"

  def renderHtml(page: String) = println(page)

  class Connection {
    def get(url: String): String = {
      val random = new Random(System.nanoTime())
      if (random.nextBoolean()) "<html> ... </html>"
      else throw new RuntimeException("Connection interrupted")
    }
  }

  object HttpService {
    val random = new Random(System.nanoTime())

    def getConnection(host: String, port: String): Connection = {
      if (random.nextBoolean()) new Connection
      else throw new RuntimeException("Someone else took the port")
    }
  }

  val connTry = Try(HttpService.getConnection(hostName, port))

  connTry
    .flatMap(conn => Try(conn.get("hostName")))
    //.foreach(renderHtml)

  for {
    connectionTry <- Try(HttpService.getConnection(hostName, port))
    result <- Try(connectionTry.get("hostName"))
  } yield renderHtml(result)


}
