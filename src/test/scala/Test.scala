import java.time.{ LocalDate, ZonedDateTime, ZoneId }

import icalendar.ical.Writer._

import net.ruippeixotog.scalascraper.browser.JsoupBrowser

import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

import org.scalatest._

class Test extends WordSpec with Main with Matchers {
  "The HTML scraping algorithm" should {
    "correctly find links in the sitemap" in {
      val doc = browser.parseResource("/sitemap.html")
      links(doc).size should be(178)
    }

    "correctly convert a details page to an event" in {
      val doc = browser.parseResource("/3js.html")
      val event = parseEvent("https://deventerschouwburg.nl/programma/3js-u2-joshua-tree-1987-a-legendary-album", doc)
      event.uid.value.text should equal("deventerschouwburg2ical-3js-u2-joshua-tree-1987-a-legendary-album")
      event.summary.get.value.text should equal("3JS - U2, The Joshua Tree (1987) a legendary album")
      event.dtstart.get.value.value.left.get.dt should equal(ZonedDateTime.of(2017, 6, 2, 20, 0, 0, 0, ZoneId.of("Europe/Amsterdam")))
    }

    "correctly convert a details page to an event without subtitle" in {
      val doc = browser.parseResource("/ademnood.html")
      val event = parseEvent("https://deventerschouwburg.nl/programma/ademnood-serious-request-deventer-2016", doc)
      event.uid.value.text should equal("deventerschouwburg2ical-ademnood-serious-request-deventer-2016")
      event.summary.get.value.text should equal("ADEMNOOD")
      event.dtstart.get.value.value.left.get.dt should equal(ZonedDateTime.of(2016, 12, 20, 20, 30, 0, 0, ZoneId.of("Europe/Amsterdam")))
      event.description.get.value.text should include("Een muzikale topavond")
    }

    "correctly parse an event with only a date range, no time" in {
      val doc = browser.parseResource("/blushhh.html")
      val event = parseEvent("https://deventerschouwburg.nl/programma/blushhh", doc)
      event.summary.get.value.text should equal("Blushhh - Flower Power")
      event.dtstart.get.value.value.right.get.d should equal(LocalDate.of(2017, 5, 19))
      event.description.get.value.text should include("Internationale artiesten")
    }

    "correctly parse an event with date and time split over 2 paragraphs" in {
      val doc = browser.parseResource("/blushhh2.html")
      val event = parseEvent("https://deventerschouwburg.nl/programma/blushhh", doc)
      event.summary.get.value.text should equal("Blushhh - Flower Power")
      event.dtstart.get.value.value.left.get.dt should equal(ZonedDateTime.of(2017, 5, 19, 21, 0, 0, 0, ZoneId.of("Europe/Amsterdam")))
      event.description.get.value.text should include("Internationale artiesten")
    }
  }
}
