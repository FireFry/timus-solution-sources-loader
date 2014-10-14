import com.burakovv.timus._
import scala.App

object Test extends App {
  val id = Integer.parseInt(System.getProperty("author"))
  val login = System.getProperty("login")
  val password = System.getProperty("password")

  def findBest(submissions: Seq[Submission]) = ???

  def saveSource(source: Source) = ???

  for {
    profile <- Timus.profile(Author(id))
    problem <- profile.problems if problem.status == ProblemStatus.Accepted
    bestTry <- findBest(Timus.submissions(problem))
    source <- Timus.source(bestTry, Credentials(login, password))
  } saveSource(source)
}