package schemadotorg

import java.time._

import net.ruippeixotog.scalascraper.model._

import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._

case class Event(name: String, performer: String, description: String, startDate: LocalDateTime)
object Event {
  def apply(doc: Document): Event = {
    Event(
      eventProperty(doc, "name").map(_.text).getOrElse(""),
      eventProperty(doc, "performer").map(_.text).getOrElse(""),
      eventProperty(doc, "description").map(_.text).getOrElse(""),
      eventProperty(doc, "startDate").map(_.attr("content")).map(LocalDateTime.parse).get)
  }

  def eventProperty(doc: Document, property: String): Option[Element] = {
    val candidates = doc >> elementList(s"""[itemprop="$property"]""")
    candidates.find(isEventProperty)
  }

  def isEventProperty(element: Element): Boolean = {
    if (element.hasAttr("itemscope")) {
      element.attr("itemtype") == "http://schema.org/Event"
    } else {
      element.parent.map(isEventProperty).getOrElse(false)
    }
  }
}
