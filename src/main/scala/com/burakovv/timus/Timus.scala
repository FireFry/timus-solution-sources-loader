package com.burakovv.timus

import org.jsoup.Jsoup
import org.jsoup.nodes.{Node, Document, TextNode}
import scala.collection.immutable

object Timus {
  def profile(author: Author): Option[Profile] = Profile.parse(loadPage(profileUrl(author)))
  def loadPage(url: String): Document = Jsoup.connect(url).get()
  def profileUrl(author: Author): String = s"http://acm.timus.ru/author.aspx?id=${author.id}"
}

case class Author(id: Int)

case class Problem(id: Int)

case class Profile private[timus](name: String, accepts: immutable.Seq[Problem])

private[timus] object Profile {
  def parse(document: Document): Option[Profile] = isKnownAuthor(document) match {
    case true => {
      val name = document.getElementsByClass("author_name").text()
      val accepts = {
        import scala.collection.JavaConversions._
        document.getElementsByClass("accepted").toIndexedSeq.map(el => Problem(el.child(0).text().toInt))
      }
      Some(new Profile(name, accepts))
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