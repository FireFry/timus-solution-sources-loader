package com.burakovv.timus

import org.jsoup.{Connection, Jsoup}
import org.jsoup.nodes.{Element, Node, Document, TextNode}
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import com.burakovv.timus.ProblemStatus.ProblemStatus
import org.jsoup.select.Elements

object Timus {
  def profile(author: Author): Option[Profile] = Profile parse {
    Jsoup.
      connect(s"http://acm.timus.ru/author.aspx?id=${author.id}").
      get()
  }

  def submissions(problemId: Int): Seq[Submission] = Submission parse {
    Jsoup.
      connect(s"http://acm.timus.ru/status.aspx?space=1&num=$problemId&author=42165&count=100").
      get()
  }

  def source(submission: Submission, credentials: Credentials): Option[Source] = Some {
    Source {
      Jsoup.
        connect(s"http://acm.timus.ru/getsubmit.aspx/${submission.id}.${submission.langExtension}").
        data("Action", "getsubmit").
        data("JudgeID", credentials.login).
        data("Password", credentials.password).
        method(Connection.Method.POST).
        execute().
        body()
    }
  }
}

case class Author(id: Int)

case class Problem(id: Int, status: ProblemStatus)

object ProblemStatus extends Enumeration {
  type ProblemStatus = Value
  val Accepted, Failed, Untouched = Value
}

case class Credentials(login: String, password: String)

case class Source(text: String)

case class Profile private[timus](name: String, problems: immutable.Seq[Problem])

object Profile {
  private[timus] def parse(document: Document): Option[Profile] = isKnownAuthor(document) match {
    case true =>
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

case class Submission(accepted: Boolean, id: Int, langExtension: String, runtime: Double)

object Submission {

  private[timus] def parse(document: Document): Seq[Submission] = {
    import scala.collection.JavaConversions._

    def parseId(elements: Element): Int = elements.child(0).text().toInt

    def parseLangExtension(element: Element): String = element.child(0).attr("href").split('.').last

    def toOption(elements: Elements): Option[Element] = if (elements.size == 0) None else Some(elements.get(0))

    def parseSubmission(el: Element): Option[Submission] = for {
      id <- toOption(el.getElementsByClass("id"))
      runtime <- toOption(el.getElementsByClass("runtime"))
      verdict <- toOption(el.getElementsByClass("verdict_ac")) orElse toOption(el.getElementsByClass("verdict_rj"))
    } yield Submission(verdict.hasClass("verdict_ac"), parseId(id), parseLangExtension(id), if (runtime.text.isEmpty) 0d else runtime.text.toDouble)

    document.getElementsByClass("status").get(0).child(0).children().flatMap(parseSubmission)
  }
}