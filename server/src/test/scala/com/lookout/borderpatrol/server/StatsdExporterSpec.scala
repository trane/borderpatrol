package com.lookout.borderpatrol.server

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.DatagramChannel

import com.lookout.borderpatrol.test.BorderPatrolSuite
import com.twitter.finagle.util.DefaultTimer
import com.twitter.io.Buf
import com.twitter.common.metrics.{AbstractGauge, Metrics}
import com.twitter.util.Duration


class StatsdExporterSpec extends BorderPatrolSuite {
  private[this] val port = 4444
  private[this] val addr = new InetSocketAddress(port)
  private[this] val server = DatagramChannel.open().bind(addr)
  server.configureBlocking(false)
  private[this] val host = s"localhost:$port"

  // StatdExporter
  val defaultStatsdExporterConfig = StatsdExporterConfig("localhost:1234", 300, "prefix")

  private[this] def receiveString: Option[String] = {
    val buf1 = ByteBuffer.allocateDirect(128)
    server.receive(buf1)
    buf1.flip()
    val buf2 = Buf.ByteBuffer.Owned(buf1)
    return Buf.Utf8.unapply(buf2)
  }

  behavior of "StatsdExporter"

  it should "instantiate with StatsExporterConfig" in {
    val metrics = new StatsdExporter(defaultStatsdExporterConfig)
    metrics.report()
  }

  it should "report counter increment" in {
    val metrics1 = Metrics.createDetached()
    val exporter1 = new StatsdExporter(metrics1, DefaultTimer.twitter,
      "ut", Duration.fromSeconds(300), host)
    val c = metrics1.createCounter("counter1")
    c.increment()
    exporter1.report()
    receiveString should be (Some("ut.counter1:1|c"))
  }

  it should "report gauge increment" in {
    val metrics2 = Metrics.createDetached()
    val exporter2 = new StatsdExporter(metrics2, DefaultTimer.twitter,
      "ut", Duration.fromSeconds(300), host)
    var x = 0
    val g = new AbstractGauge[Number]("gauge1") {
      def read: Number = x
    }
    val gauge = metrics2.registerGauge(g)
    x = 10
    exporter2.report()
    receiveString should be (Some("ut.gauge1:10|g"))
  }

  it should "report historgram increment" in {
    val metrics3 = Metrics.createDetached()
    val exporter3 = new StatsdExporter(metrics3, DefaultTimer.twitter,
      "ut", Duration.fromSeconds(300), host)
    val r = new scala.util.Random(10000)
    val histo = metrics3.createHistogram("histo")
    exporter3.report()
    receiveString should be (Some("ut.histo.count:0|g"))
    receiveString should be (Some("ut.histo.avg:0.00|t"))
    receiveString should be (Some("ut.histo.min:0|t"))
    receiveString should be (Some("ut.histo.max:0|t"))
    receiveString should be (Some("ut.histo.stddev:0.00|t"))
    receiveString should be (Some("ut.histo.p50:0|t"))
    receiveString should be (Some("ut.histo.p90:0|t"))
    receiveString should be (Some("ut.histo.p95:0|t"))
    receiveString should be (Some("ut.histo.p99:0|t"))
    receiveString should be (Some("ut.histo.p999:0|t"))
    receiveString should be (Some("ut.histo.p9999:0|t"))
  }
}
