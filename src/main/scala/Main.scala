import java.time.{ ZonedDateTime, ZoneId, ZoneOffset, LocalDateTime }
import java.io.{ InputStream, OutputStream }

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

trait Main {
  implicit def liftOption[T](value: T): Option[T] = Some(value)

  def links(doc: Document): List[String] = {
    (doc >> elementList("h3")).filter(text(_) == "Producties")
      .flatMap(_.parent >> elementList("a")).flatten
      .map(_.attr("href"))
  }

  val months = List(
    "januari",
    "februari",
    "maart",
    "april",
    "mei",
    "juni",
    "juli",
    "augustus",
    "september",
    "oktober",
    "november",
    "december")

  def parseDate(txt: String): ZonedDateTime = {
    val re = (""".*? (\d+) (""" + months.mkString("|") + """) (\d+).*?(\d+):(\d+) uur.*""").r
    txt match {
      case re(day, month, year, hour, minute) =>
        ZonedDateTime.of(year.toInt, months.indexOf(month) + 1, day.toInt, hour.toInt, minute.toInt, 0, 0, ZoneId.of("Europe/Amsterdam"))
    }
  }

  def parseEvent(link: String, doc: Document): Event = {
    val id = ".*/programma/(.*)".r.findFirstMatchIn(link).get.group(1)
    val mainContent = doc >> element(".maincontent-bar")
    val summary = (mainContent >> text("h1")) + (mainContent >?> text("h2")).map(" - " + _).getOrElse("")
    val description = (mainContent >> texts("p")).tail.head
    val startDate = parseDate((doc >> element(".prices-info") >> "p").mkString)
    val zone = ZoneId.of("Europe/Amsterdam")
    Event(
      uid = Uid(s"deventerschouwburg2ical-$id"),
      dtstart = Dtstart(startDate),
      summary = Summary(summary),
      description = Description(description),
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

  def fetchEvents(): Future[List[Event]] =
    fetchDocument(domain + "/sitemap")
      .map(links)
      .flatMap(links => Future.sequence(links.map(link =>
        fetchDetails(link).recoverWith { case t => Future.failed(new IllegalStateException(s"Could not fetch details for $link", t)) })))

  def events = Await.result(fetchEvents(), 50 seconds)
    .filter(!_.summary.get.value.text.contains("Theaterdiner reservering"))
    .filter(!_.summary.get.value.text.contains("Inleiding Orkest van het Oosten"))

  def fetchCalendar(): String = asIcal(Calendar(
    prodid = Prodid("-//raboof/deventerschouwburg2ical//NONSGML v1.0//NL"),
    events = events
  ))
}

class MainLambda extends Main {
  def handleRequest(inputStream: InputStream, outputStream: OutputStream): Unit =
    outputStream.write(fetchCalendar().getBytes("UTF-8"));
}

object MainApp extends App with Main {
  print(fetchCalendar())
}
