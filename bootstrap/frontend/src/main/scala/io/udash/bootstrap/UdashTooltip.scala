package io.udash.bootstrap

import io.udash.wrappers.jquery._
import org.scalajs.dom

import scala.concurrent.duration.{Duration, DurationInt}
import scala.language.postfixOps
import scala.scalajs.js

class UdashTooltip(selector: UdashTooltip.UdashTooltipJQuery) {
  def show(): Unit =
    selector.tooltip("show")

  def hide(): Unit =
    selector.tooltip("hide")

  def toggle(): Unit =
    selector.tooltip("toggle")

  def destroy(): Unit =
    selector.tooltip("destroy")
}

object UdashTooltip {
  case class Delay(show: Duration, hide: Duration)
  case class Viewport(selector: String, padding: Int)

  sealed abstract class Placement(val name: String)
  case object AutoPlacement extends Placement("auto")
  case object TopPlacement extends Placement("top")
  case object BottomPlacement extends Placement("bottom")
  case object LeftPlacement extends Placement("left")
  case object RightPlacement extends Placement("right")

  sealed abstract class Trigger(val name: String)
  case object ClickTrigger extends Trigger("click")
  case object HoverTrigger extends Trigger("hover")
  case object FocusTrigger extends Trigger("focus")
  case object ManualTrigger extends Trigger("manual")

  def apply(animation: Boolean = true,
            container: Option[String] = None,
            delay: Delay = Delay(0 millis, 0 millis),
            html: Boolean = false,
            placement: (dom.Element, dom.Element) => Seq[Placement] = (_, _) => Seq(TopPlacement),
            selector: Option[String] = None,
            template: Option[String] = None,
            title: (dom.Element) => String = (_) => "",
            trigger: Seq[Trigger] = Seq(HoverTrigger, FocusTrigger),
            viewport: Viewport = Viewport("body", 0))(el: dom.Node): UdashTooltip = {
    val tp: UdashTooltipJQuery = jQ(el).asTooltip()
    tp.tooltip(
      js.Dictionary(
        "animation" -> animation,
        "container" -> container.getOrElse(false),
        "delay" -> js.Dictionary("show" -> delay.show.toMillis, "hide" -> delay.hide.toMillis),
        "html" -> html,
        "placement" -> scalajs.js.Any.fromFunction2((tooltip: dom.Element, trigger: dom.Element) => placement(tooltip, trigger).map(_.name).mkString(" ")),
        "selector" -> selector.getOrElse(false),
        "template" -> template.getOrElse("<div class=\"tooltip\" role=\"tooltip\"><div class=\"tooltip-arrow\"></div><div class=\"tooltip-inner\"></div></div>"),
        "title" -> js.ThisFunction.fromFunction1(title),
        "trigger" -> trigger.map(_.name).mkString(" "),
        "viewport" -> js.Dictionary("selector" -> viewport.selector, "padding" -> viewport.padding)
      )
    )
    new UdashTooltip(tp)
  }

  @js.native
  trait UdashTooltipJQuery extends JQuery {
    def tooltip(arg: js.Any): UdashTooltipJQuery = js.native
  }

  implicit class JQueryTooltipExt(jQ: JQuery) {
    def asTooltip(): UdashTooltipJQuery =
      jQ.asInstanceOf[UdashTooltipJQuery]
  }
}
