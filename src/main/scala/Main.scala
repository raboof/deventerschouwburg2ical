import java.time.{ ZonedDateTime, ZoneId, ZoneOffset }

import scala.language.{ postfixOps, implicitConversions }

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import icalendar._
import icalendar.Properties._
import icalendar.CalendarProperties._
import icalendar.ical.Writer._


import net.ruippeixotog.scalascraper.model._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser

import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._

object Main extends App {
  implicit def liftOption[T](value: T): Option[T] = Some(value)

  def links(doc: Document): List[String] =
    (doc >> elementList(".itemTitle")).map(_ >> attr("href")("a"))

  def parseEvent(link: String, doc: Document): Event = {
    val eventData = schemadotorg.Event(doc)
    val id = ".*/programma/(\\d+)".r.findFirstMatchIn(link).get.group(1)
    val summary = eventData.performer + " - " + eventData.name
    val zone = ZoneId.of("Europe/Amsterdam")
    Event(
      uid = Uid(s"deventerschouwburg2ical-$id"),
      dtstart = Dtstart(ZonedDateTime.ofLocal(eventData.startDate, ZoneId.of("Europe/Amsterdam"), null)),
      summary = Summary(summary),
      description = Description(eventData.description),
      url = Url(link)
    )
  }

  val domain = "https://www.deventerschouwburg.nl"
  val browser = JsoupBrowser()

  def fetchDetails(url: String): Future[Event] =
    fetchDocument(url).map(doc => parseEvent(url, doc))

  def fetchDocument(url: String): Future[Document] = Future {
    browser.get(url)
  }

  def fetchIndex(url: String): Future[List[Event]] =
    fetchDocument(url)
      .map(links)
      .flatMap(links => Future.sequence(links.map(domain + _).map(fetchDetails)))

  val events = Await.result(fetchIndex(domain + "/programma"), 20 seconds)
    .filter(!_.summary.get.value.text.contains("Theaterdiner reservering"))

  print(asIcal(Calendar(
    prodid = Prodid("-//raboof/deventerschouwburg2ical//NONSGML v1.0//NL"),
    events = events
  )))
}
