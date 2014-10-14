package com.burakovv.timus

import org.jsoup.Jsoup
import org.jsoup.nodes.{Node, Document, TextNode}
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import com.burakovv.timus.ProblemStatus.ProblemStatus

object Timus {
  def profile(author: Author): Option[Profile] = Profile.parse(loadPage(profileUrl(author)))
  def loadPage(url: String): Document = Jsoup.connect(url).get()
  def profileUrl(author: Author): String = s"http://acm.timus.ru/author.aspx?id=${author.id}"
}

case class Author(id: Int)

case class Problem(id: Int, status: ProblemStatus)

case class Profile private[timus](name: String, problems: immutable.Seq[Problem])

object ProblemStatus extends Enumeration {
  type ProblemStatus = Value
  val Accepted, Failed, Untouched = Value
}

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