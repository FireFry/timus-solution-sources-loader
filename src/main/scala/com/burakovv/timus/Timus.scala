package com.burakovv.timus

import org.jsoup.Jsoup

object Timus {
  def profile(author: Author) = new Profile(author)
}

case class Author(id: Int) {
  override def toString: String = String.valueOf(id)
}

class Profile private[timus] (author: Author) extends JsoupConnection {
  override val documentUrl = s"http://acm.timus.ru/author.aspx?id=$author"
}

private[timus] trait JsoupConnection {
  private[timus] lazy val document = loadDocument()
  private[timus] def loadDocument() = Jsoup.connect(documentUrl).get()
  private[timus] def documentUrl: String

  override def toString: String = document.toString
}
