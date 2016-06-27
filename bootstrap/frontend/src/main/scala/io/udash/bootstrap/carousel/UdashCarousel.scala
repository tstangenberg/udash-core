package io.udash.bootstrap
package carousel

import io.udash._
import io.udash.bootstrap.UdashBootstrap.ComponentId
import io.udash.bootstrap.carousel.UdashCarousel.AnimationOptions.{Hover, PauseOption}
import io.udash.bootstrap.carousel.UdashCarousel.{AnimationOptions, CarouselEvent}
import io.udash.bootstrap.utils.Icons
import io.udash.wrappers.jquery.JQuery
import org.scalajs.dom
import org.scalajs.dom.Element

import scala.concurrent.ExecutionContext
import scala.language.postfixOps
import scala.scalajs.js
import scalacss.ScalatagsCss._
import scalatags.JsDom.all._

class UdashCarousel(val content: SeqProperty[UdashCarouselSlide], componentId: ComponentId,
                    showIndicators: Boolean, activeSlide: Int, animationOptions: AnimationOptions)
                   (implicit ec: ExecutionContext) extends UdashBootstrapComponent with Listenable[UdashCarousel, CarouselEvent] {

  import BootstrapStyles.Carousel._
  import BootstrapTags._
  import UdashCarousel._
  import io.udash.wrappers.jquery._

  private lazy val indices = content.transform((slides: Seq[UdashCarouselSlide]) => slides.length)
  private lazy val _activeIndex: Property[Int] = Property[Int](firstActive)

  override lazy val render: Element = {
    def indicators() = {
      def indicator(index: Int) =
        li(dataTarget := s"#$carouselId", dataSlideTo := index, BootstrapStyles.active.styleIf(activeIndex.transform(idx => idx == index)))

      produce(indices)(length =>
        ol(carouselIndicators)(
          (0 until length).map(indicator).render
        ).render
      )
    }

    val counter = new Countdown(firstActive)
    val res = div(id := carouselId, carousel, slide)(
      if (showIndicators) indicators() else {},
      div(carouselInner, role := "listbox")(
        repeat(content) { slide =>
          val res = slide.get.render
          if (counter.left() == 0) BootstrapStyles.active.applyTo(res)
          res
        }
      ),
      a(BootstrapStyles.left, carouselControl, href := s"#$carouselId", role := "button", dataSlide := "prev")(
        Icons.FontAwesome.chevronLeft,
        span(`class` := "sr-only", "Previous")
      ),
      a(BootstrapStyles.right, carouselControl, href := s"#$carouselId", role := "button", dataSlide := "next")(
        Icons.FontAwesome.chevronRight,
        span(`class` := "sr-only", "Next")
      )
    ).render
    val jq = jQ(res)
    val jqCarousel = jq.asCarousel()
    jqCarousel.on("slid.bs.carousel", (_: dom.Element, ev: JQueryEvent) =>
      _activeIndex.set(content.get.iterator.zipWithIndex.collectFirst {
        case (sl: UdashCarouselSlide, idx: Int) if sl.render == ev.relatedTarget => idx
      }.get
      )
    )
    jqCarousel.carousel(animationOptions)
    if (!animationOptions.active) jqCarousel.pause()
    res
  }

  def activeIndex: ReadableProperty[Int] = _activeIndex.transform(identity)

  def cycle(): Unit = jQSelector().cycle()

  def pause(): Unit = jQSelector().pause()

  def goTo(slideNumber: Int): Unit = jQSelector().goTo(slideNumber)

  def nextSlide(): Unit = jQSelector().nextSlide()

  def previousSlide(): Unit = jQSelector().previousSlide()

  private def jQSelector(): UdashCarouselJQuery = jQ(render).asCarousel()

  def carouselId: String = s"$componentId-carousel"

  private def firstActive: Int = math.min(activeSlide, content.length - 1)

  private class Countdown(private var ticks: Int) {
    def left(): Int =
      if (ticks > -1) {
        val res = ticks
        ticks -= 1
        res
      } else ticks
  }


}

object UdashCarousel {

  def apply(content: SeqProperty[UdashCarouselSlide], componentId: ComponentId = UdashBootstrap.newId(),
            showIndicators: Boolean = true, activeSlide: Int = 0, animationOptions: AnimationOptions = AnimationOptions())
           (implicit ec: ExecutionContext): UdashCarousel =
    new UdashCarousel(content, componentId, showIndicators, activeSlide, animationOptions)

  sealed trait CarouselEvent extends ListenableEvent[UdashCarousel]

  @js.native
  private trait UdashCarouselJQuery extends JQuery {
    def carousel(options: CarouselOptionsJS): UdashCarouselJQuery = js.native

    def carousel(cmd: String): UdashCarouselJQuery = js.native

    def carousel(number: Int): UdashCarouselJQuery = js.native
  }

  import scala.concurrent.duration._

  @js.native
  private trait CarouselOptionsJS extends js.Object {
    var interval: Int = js.native
    var pause: String = js.native
    var wrap: Boolean = js.native
    var keyboard: Boolean = js.native
  }

  case class AnimationOptions(interval: Duration = 5 seconds, pause: PauseOption = Hover, wrap: Boolean = true,
                              keyboard: Boolean = true, active: Boolean = true) {
    private[UdashCarousel] def native: CarouselOptionsJS = {
      val options = js.Object().asInstanceOf[CarouselOptionsJS]
      options.interval = interval.toMillis.toInt
      options.pause = pause.raw
      options.wrap = wrap
      options.keyboard = keyboard
      options
    }
  }

  case class SlideChangeEvent(source: UdashCarousel) extends CarouselEvent

  case class SlideChangedEvent(source: UdashCarousel) extends CarouselEvent

  private implicit class UdashCarouselJQueryExt(jQ: JQuery) {
    def asCarousel(): UdashCarouselJQuery = jQ.asInstanceOf[UdashCarouselJQuery]
  }

  private implicit class UdashCarouselJQueryOps(jq: UdashCarouselJQuery) {

    def carousel(animationOptions: AnimationOptions): Unit = jq.carousel(animationOptions.native)

    def cycle(): Unit = jq.carousel("cycle")

    def pause(): Unit = jq.carousel("pause")

    def goTo(slideNumber: Int): Unit = jq.carousel(slideNumber)

    def nextSlide(): Unit = jq.carousel("next")

    def previousSlide(): Unit = jq.carousel("prev")

  }

  object AnimationOptions {

    sealed abstract class PauseOption(val raw: String)

    case object Hover extends PauseOption("hover")

    case object False extends PauseOption("false")

  }
}

case class UdashCarouselSlide(imgSrc: Url)(content: Modifier*) extends UdashBootstrapComponent {
  override lazy val render: Element = div(BootstrapStyles.item)(
    img(src := imgSrc.value),
    div(BootstrapStyles.Carousel.carouselCaption)(
      content
    )
  ).render

}