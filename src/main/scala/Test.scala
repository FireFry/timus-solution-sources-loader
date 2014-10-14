import com.burakovv.timus.{Author, Timus}
import scala.App

object Test extends App {
  val id = Integer.parseInt(System.getProperty("author"))
  Timus.profile(Author(id)).map {
    println
  }
}