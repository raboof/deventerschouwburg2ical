import java.time.{ ZonedDateTime, ZoneId }

import Main._

import icalendar.ical.Writer._

import net.ruippeixotog.scalascraper.browser.JsoupBrowser

import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

import org.scalatest._

class Test extends WordSpec with Matchers {
  val browser = JsoupBrowser()

  "The HTML scraping algorithm" should {
    "correctly find links in programma.html" in {
      val doc = browser.parseResource("/programma.html")
      val links = Main.links(doc)
      links.size should be(8)
    }

    "correctly convert a details page to an event" in {
      val doc = browser.parseResource("/draai_het_eens_om.html")
      val event = Main.parseEvent("http://www.deventerschouwburg.nl/programma/1869/nuhr/draai_het_eens_om", doc)
      event.uid.value.text should equal("deventerschouwburg2ical-1869")
      event.summary.get.value.text should equal("Draai het eens om - NUHR")
      event.dtstart.get.value.dt should equal(ZonedDateTime.of(2016, 10, 19, 20, 0, 0, 0, ZoneId.of("Europe/Amsterdam")))
    }
  }
}
