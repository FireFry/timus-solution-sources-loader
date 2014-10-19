import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.burakovv.timus._

object Test extends App {
  val id = Integer.parseInt(System.getProperty("author"))
  val login = System.getProperty("login")
  val password = System.getProperty("password")
  val dir = System.getProperty("outputDir")
  var sources = 0

  def findBest(submissions: Seq[Submission]): Option[Submission] = Some(submissions.foldLeft(submissions(0))((min: Submission, s: Submission) => {
    if (s.accepted && s.runtime < min.runtime) s else min
  }))

  def saveSource(problemId: Int, submission: Submission, source: Source) = {
    sources += 1
    println(s"Saving source $sources for $submission")
    Files.write(Paths.get(s"${dir}$problemId.${submission.langExtension}"), source.text.getBytes(StandardCharsets.UTF_8))
  }

  val solvedProblems: Seq[Int] = new File(dir).list().filter(_.matches( """[0-9]+\.\w+""")).map(name => name.split('.')(0).toInt)

  for {
    profile <- Timus.profile(Author(id))
    problemId <- profile.problems.filter(_.status == ProblemStatus.Accepted).map(_.id).diff(solvedProblems)
    bestTry <- findBest(Timus.submissions(problemId))
    source <- Timus.source(bestTry, Credentials(login, password))
  } saveSource(problemId, bestTry, source)
}
