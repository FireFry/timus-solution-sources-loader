package com.burakovv.timus

import org.jsoup.{Connection, Jsoup}
import org.jsoup.nodes.{Node, Document, TextNode}
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import com.burakovv.timus.ProblemStatus.ProblemStatus

object Timus {
  def profile(author: Author): Option[Profile] = Profile parse {
    Jsoup.
      connect(s"http://acm.timus.ru/author.aspx?id=${author.id}").
      get()
  }

  def submissions(problem: Problem): Seq[Submission] = ???

  def source(submission: Submission, credentials: Credentials): Source = Source {
    Jsoup.
      connect(s"http://acm.timus.ru/getsubmit.aspx/${submission.id}.java").
      data("Action", "getsubmit").
      data("JudgeID", credentials.login).
      data("Password", credentials.password).
      method(Connection.Method.POST).
      execute().
      body()
  }
}

case class Author(id: Int)

case class Problem(id: Int, status: ProblemStatus)

case class Profile private[timus](name: String, problems: immutable.Seq[Problem])

object ProblemStatus extends Enumeration {
  type ProblemStatus = Value
  val Accepted, Failed, Untouched = Value
}

case class Credentials(login: String, password: String)

case class Submission(accepted: Boolean, id: Int, langExtension: String, runtime: Double)

case class Source(text: String)

private[timus] object Profile {
  def parse(document: Document): Option[Profile] = isKnownAuthor(document) match {
    case true => {
      val name = document.getElementsByClass("author_name").text()
      val problems = {
        import scala.collection.JavaConversions._
        import com.burakovv.timus.ProblemStatus._

        val builder = new ArrayBuffer[Problem]()

        def parseProblems(elementClass: String, status: ProblemStatus) =
          document.getElementsByClass(elementClass).map(e => Problem(e.child(0).text().toInt, status))

        builder ++= parseProblems("accepted", Accepted)
        builder ++= parseProblems("tried", Failed)
        builder ++= parseProblems("empty", Untouched)

        builder.toIndexedSeq
      }
      Some(new Profile(name, problems))
    }
    case false => None
  }

  private def isKnownAuthor(profilePage: Document) = {
    val childNode: Node = profilePage.body().child(0).child(0).child(2).child(0).child(0).childNode(0)
    childNode match {
      case t: TextNode if t.text == "Author not found" => false
      case _ => true
    }
  }
}