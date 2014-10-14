import com.burakovv.timus._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.App

object Test extends App {
  val id = Integer.parseInt(System.getProperty("author"))
  val login = System.getProperty("login")
  val password = System.getProperty("password")
  val dir = System.getProperty("outputDir")
  var sources = 0

  def findBest(submissions: Seq[Submission]): Option[Submission] = Some(submissions.foldLeft(submissions(0))((min: Submission, s: Submission) => {
    if (s.accepted && s.runtime < min.runtime) s else min
  }))

  def saveSource(problem: Problem, submission: Submission, source: Source) = {
    sources += 1
    println(s"Saving source $sources for $submission")
    Files.write(Paths.get(s"${dir}${problem.id}.${submission.langExtension}"), source.text.getBytes(StandardCharsets.UTF_8))
  }

  for {
    profile <- Timus.profile(Author(id))
    problem <- profile.problems if problem.status == ProblemStatus.Accepted
    bestTry <- findBest(Timus.submissions(problem))
    source <- Timus.source(bestTry, Credentials(login, password))
  } saveSource(problem, bestTry, source)
}
