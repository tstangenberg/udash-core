package io.udash.testing

import java.io.PrintWriter
import java.util
import java.util.concurrent.{Future, TimeUnit}
import javax.servlet.http.HttpSession

import io.udash.rpc.{AtmosphereServiceConfig, ExposesServerRPC, Utils}
import org.atmosphere.cpr.AtmosphereResource.TRANSPORT
import org.atmosphere.cpr._
import org.scalatest.concurrent.Eventually
import org.scalatest.time.{Millis, Span}

import scala.collection.mutable
import scala.util.Try

trait UdashRpcBackendTest extends UdashSharedTest with Utils with Eventually {

  override implicit val patienceConfig = PatienceConfig(scaled(Span(5000, Millis)), scaled(Span(100, Millis)))

  class BroadcasterMock extends DefaultBroadcaster {
    val broadcasts = mutable.ListBuffer[String]()
    val addedResources = mutable.ListBuffer[AtmosphereResource]()

    override def broadcast(msg: scala.Any): Future[AnyRef] = {
      broadcasts += msg.toString
      null
    }

    override def addAtmosphereResource(r: AtmosphereResource): Broadcaster = {
      addedResources += r
      this
    }
  }

  class BroadcasterFactoryMock(broadcaster: Broadcaster) extends DefaultBroadcasterFactory {
    val lookups = mutable.ListBuffer[String]()

    private def _lookup[T <: Broadcaster](msg: String): T = {
      lookups += msg
      broadcaster.asInstanceOf[T]
    }

    override def lookup[T <: Broadcaster](c: Class[T], id: scala.Any): T = _lookup(id.toString)
    override def lookup[T <: Broadcaster](c: Class[T], id: scala.Any, createIfNull: Boolean): T = _lookup(id.toString)
    override def lookup[T <: Broadcaster](id: scala.Any): T = _lookup(id.toString)
    override def lookup[T <: Broadcaster](id: scala.Any, createIfNull: Boolean): T = _lookup(id.toString)
  }

  class MetaBroadcasterMock extends DefaultMetaBroadcaster {
    val broadcasts = mutable.ListBuffer[(String, String)]()

    override def broadcastTo(broadcasterID: String, message: scala.Any): Future[util.List[Broadcaster]] = {
      broadcasts.+=((broadcasterID, message.toString))
      null
    }
  }

  class AtmosphereServiceConfigMock[RPCType](override val filters: Seq[(AtmosphereResource) => Try[Any]] = Seq.empty,
                                             resolveRpcResult: ExposesServerRPC[RPCType] = null)
    extends AtmosphereServiceConfig[RPCType] {
    var closed = false

    override def initRpc(resource: AtmosphereResource): Unit = ()
    override def resolveRpc(resource: AtmosphereResource): ExposesServerRPC[RPCType] = resolveRpcResult
    override def onClose(resource: AtmosphereResource): Unit = {
      closed = true
    }
  }

  class AtmosphereConfigMock(broadcasterFactory: BroadcasterFactory, _metaBroadcaster: MetaBroadcaster) extends AtmosphereConfig(null) {
    override def getBroadcasterFactory: BroadcasterFactory = broadcasterFactory
    override def metaBroadcaster(): MetaBroadcaster = _metaBroadcaster
  }

  class AtmosphereResourceMock(_transport: TRANSPORT, _uuid: String, _request: AtmosphereRequest, _response: AtmosphereResponse = null) extends AtmosphereResourceImpl {

    var suspeded = false
    var resumed = false
    var mockBroadcaster: Broadcaster = null

    override def getBroadcaster: Broadcaster = mockBroadcaster

    override def isSuspended: Boolean = suspeded

    override def getRequest: AtmosphereRequest = _request

    override def isResumed: Boolean = resumed

    override def suspend(): AtmosphereResource = {
      suspeded = true
      this
    }

    override def suspend(timeout: Long): AtmosphereResource = {
      suspeded = true
      this
    }

    override def suspend(timeout: Long, timeunit: TimeUnit): AtmosphereResource = {
      suspeded = true
      this
    }

    override def transport(): TRANSPORT = _transport

    override def setBroadcaster(_broadcaster: Broadcaster): AtmosphereResource = {
      mockBroadcaster = _broadcaster
      this
    }

    override def resume(): AtmosphereResource = {
      suspeded = false
      resumed = true
      this
    }

    override def getResponse: AtmosphereResponse = _response
    override def uuid(): String = _uuid
  }

  class AtmosphereResponseMock(writer: PrintWriter) extends AtmosphereResponseImpl(null, null, false) {
    var error = false
    var write = false

    override def sendError(sc: Int): Unit = { error = true }
    override def write(data: String): AtmosphereResponse = {
      write = true
      null
    }

    override def getWriter: PrintWriter = writer
  }
}