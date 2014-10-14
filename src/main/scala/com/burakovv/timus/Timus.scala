package com.burakovv.timus

import org.jsoup.Jsoup
import org.jsoup.nodes.{Node, Document, TextNode}

object Timus {
  def profile(author: Author): Option[Profile] = {
    val url: String = s"http://acm.timus.ru/author.aspx?id=${author.id}"
    Profile.parse(Jsoup.connect(url).get())
  }
}

case class Author(id: Int)

class Profile private[timus]() {
}

private[timus] object Profile {
  def parse(document: Document): Option[Profile] = isKnownAuthor(document) match {
    case true => Some(new Profile { override def toString: String = document.toString })
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