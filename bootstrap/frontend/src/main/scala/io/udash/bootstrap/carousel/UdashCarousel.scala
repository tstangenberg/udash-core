package io.udash.bootstrap
package carousel

import io.udash._
import io.udash.bootstrap.UdashBootstrap.ComponentId
import io.udash.bootstrap.carousel.UdashCarousel.CarouselEvent
import io.udash.bootstrap.utils.Icons
import org.scalajs.dom.Element

import scala.concurrent.ExecutionContext
import scalacss.ScalatagsCss._
import scalatags.JsDom.all._

class UdashCarousel(val content: SeqProperty[UdashCarouselSlide], carouselId: ComponentId, val showIndicators: Boolean)
                   (implicit ec: ExecutionContext) extends UdashBootstrapComponent with Listenable[UdashCarousel, CarouselEvent] {

  import BootstrapStyles.Carousel._
  import BootstrapTags._

  //val active: Property[Int] = Property[Int](0)

  private lazy val indices = content.transform((slides: Seq[UdashCarouselSlide]) => slides.length)

  private def indicators() = {
    def indicator(index: Int) = {
      li(dataTarget := s"#$carouselId-carousel", dataSlideTo := index, BootstrapStyles.active.styleIf(index == 0))
    }
    produce(indices)(length =>
      ol(carouselIndicators)(
        (0 until length).map(indicator).render
      ).render
    )
  }

  override lazy val render: Element = {
    var _first = true
    div(id := s"$carouselId-carousel", carousel, slide)(
      if (showIndicators) indicators() else {},
      div(carouselInner, role := "listbox")(
        repeat(content) { slide =>
          val res = slide.get.render
          if (_first) {
            BootstrapStyles.active.applyTo(res)
            _first = false
          }
          res
        }
      ),
      a(BootstrapStyles.left, carouselControl, href := s"#$carouselId-carousel", role := "button", dataSlide := "prev")(
        Icons.FontAwesome.chevronLeft,
        span(`class` := "sr-only", "Previous")
      ),
      a(BootstrapStyles.right, carouselControl, href := s"#$carouselId-carousel", role := "button", dataSlide := "next")(
        Icons.FontAwesome.chevronRight,
        span(`class` := "sr-only", "Next")
      )
    ).render
  }
}

object UdashCarousel {

  sealed trait CarouselEvent extends ListenableEvent[UdashCarousel]

  case class SlideChangeEvent(source: UdashCarousel) extends CarouselEvent

  case class SlideChangedEvent(source: UdashCarousel) extends CarouselEvent

  def apply(content: SeqProperty[UdashCarouselSlide], carouselId: ComponentId = UdashBootstrap.newId(),
            showIndicators: Boolean = true)(implicit ec: ExecutionContext): UdashCarousel =
    new UdashCarousel(content, carouselId, showIndicators)
}

case class UdashCarouselSlide(imgSrc: Url)(content: Modifier*) extends UdashBootstrapComponent {
  override lazy val render: Element = div(BootstrapStyles.item)(
    img(src := imgSrc.value),
    div(BootstrapStyles.Carousel.carouselCaption)(
      content
    )
  ).render

}