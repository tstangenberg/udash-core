package io.udash.bootstrap
package pagination

import io.udash._
import io.udash.bootstrap.UdashBootstrap.ComponentId
import io.udash.properties.seq
import org.scalajs.dom
import org.scalajs.dom.Event

import scala.concurrent.ExecutionContext

sealed trait PaginationComponent[PageType, ElemType <: Property[PageType]] extends UdashBootstrapComponent {
  /** Sequence of pagination elements. Pagination will automatically synchronize with this property changes. */
  def pages: seq.SeqProperty[PageType, ElemType]

  /** Index of selected page. */
  def selectedPage: Property[Int]

  /** Safely set selected page to the provided index. Will change index if it is out of bounds. */
  def changePage(pageIdx: Int): Unit = {
    import math._
    selectedPage.set(min(pages.get.size - 1, max(0, pageIdx)))
  }

  /** Safely selects the next page. */
  def next(): Unit = changePage(selectedPage.get + 1)

  /** Safely selects the previous page. */
  def previous(): Unit = changePage(selectedPage.get - 1)
}

class UdashPagination[PageType, ElemType <: Property[PageType]] private
                     (size: PaginationSize, showArrows: Property[Boolean], highlightActive: Property[Boolean], override val componentId: ComponentId)
                     (val pages: seq.SeqProperty[PageType, ElemType], val selectedPage: Property[Int])
                     (itemFactory: (ElemType, UdashPagination.ButtonType) => dom.Element)(implicit ec: ExecutionContext)
  extends PaginationComponent[PageType, ElemType] {

  override lazy val render: dom.Element = {
    import scalatags.JsDom.all._
    import scalatags.JsDom.tags2

    tags2.nav(
      ul(id := componentId, BootstrapStyles.Pagination.pagination)(
        arrow((idx, _) => idx <= 0, previous, UdashPagination.PreviousPage),
        repeat(pages)(page => {
          def currentIdx: Int = pages.elemProperties.indexOf(page)
          val pageIdx = Property[Int](currentIdx)
          pages.listen(_ => pageIdx.set(currentIdx))
          li(BootstrapStyles.active.styleIf(selectedPage.combine(pageIdx)(_ == _).combine(highlightActive)(_ && _)))(
            itemFactory(page, UdashPagination.StandardPage)
          )(onclick :+= ((_: Event) => { changePage(pageIdx.get); false })).render
        }),
        arrow((idx, size) => idx >= size - 1, next, UdashPagination.NextPage)
      )
    ).render
  }

  protected def arrow(highlightCond: (Int, Int) => Boolean, onClick: () => Any, buttonType: UdashPagination.ButtonType) = {
    import scalatags.JsDom.all._

    produce(showArrows.combine(pages)((_, _))) {
      case (true, _) =>
        val elements = pages.elemProperties
        li(BootstrapStyles.disabled.styleIf(selectedPage.transform((idx: Int) => highlightCond(idx, elements.size))))(
          produce(selectedPage)(idx => itemFactory(elements(math.min(elements.size - 1, idx + 1)), buttonType))
        )(onclick :+= ((_: Event) => { onClick(); false })).render
      case (false, _) =>
        span().render
    }
  }
}

class UdashPager[PageType, ElemType <: Property[PageType]] private[pagination](aligned: Boolean, override val componentId: ComponentId)
                (val pages: seq.SeqProperty[PageType, ElemType], val selectedPage: Property[Int])
                (itemFactory: (ElemType, UdashPagination.ButtonType) => dom.Element) extends PaginationComponent[PageType, ElemType] {

  override lazy val render: dom.Element = {
    import scalatags.JsDom.all._
    import scalatags.JsDom.tags2

    tags2.nav(id := componentId)(
      ul(BootstrapStyles.Pagination.pager)(
        arrow((idx, _) => idx <= 0, previous, UdashPagination.PreviousPage, BootstrapStyles.previous),
        arrow((idx, size) => idx >= size - 1, next, UdashPagination.NextPage, BootstrapStyles.next)
      )
    ).render
  }

  protected def arrow(highlightCond: (Int, Int) => Boolean, onClick: () => Any, buttonType: UdashPagination.ButtonType, alignStyle: BootstrapStyles.BootstrapClass) = {
    import scalatags.JsDom.all._

    produce(pages)(_ => {
      val elements = pages.elemProperties
      li(
        BootstrapStyles.disabled.styleIf(selectedPage.transform((idx: Int) => highlightCond(idx, elements.size))),
        alignStyle.styleIf(aligned)
      )(
        produce(selectedPage)(idx => itemFactory(elements(math.min(elements.size - 1, idx + 1)), buttonType))
      )(onclick :+= ((_: Event) => { onClick(); false })).render
    })
  }
}

object UdashPagination {
  import scalatags.JsDom.all._

  sealed trait ButtonType
  case object StandardPage extends ButtonType
  case object PreviousPage extends ButtonType
  case object NextPage extends ButtonType

  /** Default pagination element model. */
  trait Page {
    def name: String
    def url: Url
  }
  case class DefaultPage(override val name: String, override val url: Url) extends Page

  private def bindHref(page: CastableProperty[Page]) =
    bindAttribute(page.asModel.subProp(_.url))((url, el) => el.setAttribute("href", url.value))

  /** Creates link for default pagination element model. */
  val defaultPageFactory: (CastableProperty[Page], UdashPagination.ButtonType) => dom.Element = {
    case (page, UdashPagination.PreviousPage) =>
      a(aria.label := "Previous", bindHref(page))(span(aria.hidden := true)("«")).render
    case (page, UdashPagination.NextPage) =>
      a(aria.label := "Next", bindHref(page))(span(aria.hidden := true)("»")).render
    case (page, UdashPagination.StandardPage) =>
      a(bindHref(page))(bind(page.asModel.subProp(_.name))).render
  }

  import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  /**
    * Creates default pagination with pages display. More: <a href="http://getbootstrap.com/components/#pagination">Bootstrap Docs</a>.
    *
    * @param size            Pagination component size.
    * @param showArrows      If property value is true, shows next/prev page arrows.
    * @param highlightActive If property value is true, highlights selected page.
    * @param componentId     Id of the root DOM node.
    * @param pages           Sequence of available pages.
    * @param selectedPage    Property containing selected page index.
    * @param itemFactory     Creates button for element in pagination.
    * @tparam PageType Single element type in `items`.
    * @tparam ElemType Type of the property containing every element in `items` sequence.
    * @return `UdashPagination` component, call render to create DOM element.
    */
  def apply[PageType, ElemType <: Property[PageType]]
           (size: PaginationSize = PaginationSize.Default, showArrows: Property[Boolean] = Property(true),
            highlightActive: Property[Boolean] = Property(true), componentId: ComponentId = UdashBootstrap.newId())
           (pages: seq.SeqProperty[PageType, ElemType], selectedPage: Property[Int])
           (itemFactory: (ElemType, UdashPagination.ButtonType) => dom.Element)(implicit ec: ExecutionContext): UdashPagination[PageType, ElemType] =
    new UdashPagination(size, showArrows, highlightActive, componentId)(pages, selectedPage)(itemFactory)

  /**
    * Creates pager with next/prev buttons only. More: <a href="http://getbootstrap.com/components/#pagination">Bootstrap Docs</a>.
    *
    * @param aligned Show arrows aligned to site border.
    * @param componentId Id of the root DOM node.
    * @param pages Sequence of available pages.
    * @param selectedPage Property containing selected page index.
    * @param itemFactory Creates button for element in pagination.
    * @tparam PageType Single element type in `items`.
    * @tparam ElemType Type of the property containing every element in `items` sequence.
    * @return `UdashPagination` component, call render to create DOM element.
    */
  def pager[PageType, ElemType <: Property[PageType]]
           (aligned: Boolean = false, componentId: ComponentId = UdashBootstrap.newId())
           (pages: seq.SeqProperty[PageType, ElemType], selectedPage: Property[Int])
           (itemFactory: (ElemType, UdashPagination.ButtonType) => dom.Element)(implicit ec: ExecutionContext): UdashPager[PageType, ElemType] =
    new UdashPager(aligned, componentId)(pages, selectedPage)(itemFactory)
}