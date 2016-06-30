package io.udash.bootstrap
package scrollspy

import io.udash.bootstrap.UdashBootstrap.ComponentId
import io.udash.bootstrap.navs.UdashNavbar
import io.udash.bootstrap.scrollspy.UdashScrollspy.ElementActivated
import io.udash.wrappers.jquery._
import org.scalajs.dom

import scala.scalajs.js

class UdashScrollspy private(navId: String, spied: dom.Element, offset: Int) extends Listenable[UdashScrollspy, ElementActivated] {

  import UdashScrollspy._

  private val jQSelector: UdashScrollspyJQuery = jQ(spied).asScrollspy()
  jQSelector.css("position", "relative")
  jQSelector.target(s"#$navId", offset)
  jQSelector.on("activate.bs.scrollspy", jQFire(ElementActivated(this)))


  def refresh(): Unit =
    jQ("[data-spy=\"scroll\"]").each((el, i) => jQ(el).asScrollspy().refresh)

}

object UdashScrollspy {

  def apply(nav: UdashNavbar[_, _], spied: dom.Element = dom.document.body, offset: Int = 10): UdashScrollspy =
    new UdashScrollspy(nav.navId.id, spied, offset)

  def raw(navId: String, spied: dom.Element = dom.document.body, offset: Int = 10): UdashScrollspy =
    new UdashScrollspy(navId, spied, offset)

  @js.native
  private trait UdashScrollspyJQuery extends JQuery {
    def scrollspy(options: ScrollspyOptionsJS): UdashScrollspyJQuery = js.native

    def scrollspy(cmd: String): UdashScrollspyJQuery = js.native
  }

  @js.native
  private trait ScrollspyOptionsJS extends js.Object {
    var target: String = js.native
    var offset: Int = js.native
  }

  private implicit class UdashScrollspyJQueryExt(jQ: JQuery) {
    def asScrollspy(): UdashScrollspyJQuery = jQ.asInstanceOf[UdashScrollspyJQuery]
  }

  private implicit class UdashScrollspyJQueryOps(jq: UdashScrollspyJQuery) {

    def target(id: String, offset: Int = 10): Unit = jq.scrollspy {
      val obj = js.Object().asInstanceOf[ScrollspyOptionsJS]
      obj.target = id
      obj.offset = offset
      obj
    }

    def targetComponent(id: ComponentId, offset: Int = 10): Unit = target(id.id, offset)

    def refresh = jq.scrollspy("refresh")

  }

  case class ElementActivated(source: UdashScrollspy) extends ListenableEvent[UdashScrollspy]

}