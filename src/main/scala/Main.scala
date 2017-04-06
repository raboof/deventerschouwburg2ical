import java.time.{ LocalDate, ZonedDateTime, ZoneId, ZoneOffset, LocalDateTime }
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

object Helpers {
  import scala.util.Try

  implicit def liftOption[T](value: T): Option[T] = Some(value)

  object ToInt {
    def unapply(in: String): Option[Int] = Try(in.toInt).toOption
  }

  object ToMonth {
    def unapply(in: String): Option[Int] = in match {
      case "januari" => 1
      case "februari" => 2
      case "maart" => 3
      case "april" => 4
      case "mei" => 5
      case "juni" => 6
      case "juli" => 7
      case "augustus" => 8
      case "september" => 9
      case "oktober" => 10
      case "november" => 11
      case "december" => 12
      case _ => None
    }
  }

  implicit class RegexHelper(val sc: StringContext) extends AnyVal {
    def re: scala.util.matching.Regex = sc.parts.mkString.r
  }
}

trait Main {
  import Helpers._

  def links(doc: Document): List[String] = {
    (doc >> elementList("h3")).filter(text(_) == "Producties")
      .flatMap(_.parent >> elementList("a")).flatten
      .map(_.attr("href"))
  }

  def parseDate(txt: String): Option[Dtstart] = {
    txt match {
      case re""".*? (\d+)${ ToInt(day) } (\w+)${ ToMonth(month) } (\d+)${ ToInt(year) }.*?(\d?\d)${ ToInt(hour) }:(\d{2})${ ToInt(minute) }.*""" =>
        ZonedDateTime.of(year, month, day, hour, minute, 0, 0, ZoneId.of("Europe/Amsterdam"))
      case re""".*? (\d+)${ ToInt(day) } (\w+)${ ToMonth(month) } t/m \w+ \d+ \w+ (\d+)${ ToInt(year) }.*""" =>
        LocalDate.of(year, month, day)
    }
  }

  def parseEvent(link: String, doc: Document): Event = {
    val id = ".*/programma/(.*)".r.findFirstMatchIn(link).get.group(1)
    val mainContent = doc >> element(".maincontent-bar")
    val summary = (mainContent >> text("h1")) + (mainContent >?> text("h2")).map(" - " + _).getOrElse("")
    val description = (doc >> texts(".maincontent-bar > p")).tail.head
    val zone = ZoneId.of("Europe/Amsterdam")
    Event(
      uid = Uid(s"deventerschouwburg2ical-$id"),
      dtstart = parseDate((doc >> element(".prices-info") >> "p").mkString),
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
