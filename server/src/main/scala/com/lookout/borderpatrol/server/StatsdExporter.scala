package com.lookout.borderpatrol.server

import java.net.InetAddress
import java.nio.ByteBuffer
import java.nio.channels.DatagramChannel
import com.twitter.common.metrics.Metrics
import com.twitter.finagle.stats._
import com.twitter.finagle.util.{InetSocketAddressUtil, DefaultTimer}
import com.twitter.io.Buf
import com.twitter.util.{Duration, Timer, NonFatal}
import scala.collection.JavaConverters.mapAsScalaMapConverter
import scala.collection.Map


class StatsdExporter(registry: Metrics, timer: Timer, prefix: String = "", duration: Duration, hostAndPort: String) {
  private[this] val addr = InetSocketAddressUtil.parseHosts(hostAndPort).head
  private[this] val channel = DatagramChannel.open()

  // Alternate constructor
  def this(config: StatsdExporterConfig) = this(MetricsStatsReceiver.defaultRegistry, DefaultTimer.twitter,
    config.prefix.getOrDefault(InetAddress.getLocalHost.getHostName), Duration.fromSeconds(config.durationInSec),
    config.host)

  // Schedule exporter
  timer.schedule(duration)(report)

  // Format helpers
  private[this] def format(names: Seq[String], value: String, term: String): String = {
    val n = names.filter(_.nonEmpty).mkString(".").replaceAll("/", ".").replaceAll(":", "_")
    s"${n}:$value|$term"
  }

  private[this] def format(n: Long): String = n.toString

  private[this] def format(v: Double): String = "%2.2f".format(v)

  // Send helpers
  private[this] def buf(utf8: String): Buf =
    Buf.Utf8(utf8)

  private[this] def byteBuf(buf: Buf): ByteBuffer =
    Buf.ByteBuffer.Owned.extract(buf)

  private[this] def send(str: String): Unit = channel.send(byteBuf(buf(str)), addr)

  // Computer helpers
  private[this] def labelPercentile(p: Double): String = {
    // this has a strange quirk that p999 gets formatted as p9990
    val gname: String = "p" + (p * 10000).toInt
    if (3 < gname.length && ("00" == gname.substring(3))) {
      gname.substring(0, 3)
    } else {
      gname
    }
  }

  def report(): Unit = {
    val gauges = try registry.sampleGauges().asScala catch {
      case NonFatal(e) =>
        // because gauges run arbitrary user code, we want to protect ourselves here.
        // while the underlying registry should protect against individual misbehaving
        // gauges, an extra level of belt-and-suspenders seemed worthwhile.
        //log.error(e, "exception while collecting gauges")
        Map.empty[String, Number]
    }
    val histos = registry.sampleHistograms().asScala

    val counters = registry.sampleCounters().asScala

    counters.foreach {
      case (name, value) => send(format(Seq(prefix, name), format(value.longValue()), "c"))
    }
    gauges.foreach {
      case (name, value) => send(format(Seq(prefix, name), format(value.longValue()), "g"))
    }

    histos.foreach { case (name, snapshot) =>
      send(format(Seq(prefix, name, "count"), format(snapshot.count), "g"))
      send(format(Seq(prefix, name, "avg"), format(snapshot.avg), "t"))
      send(format(Seq(prefix, name, "min"), format(snapshot.min), "t"))
      send(format(Seq(prefix, name, "max"), format(snapshot.max), "t"))
      send(format(Seq(prefix, name, "stddev"), format(snapshot.stddev), "t"))
      snapshot.percentiles.foreach(p =>
        send(format(Seq(prefix, name, labelPercentile(p.getQuantile)), format(p.getValue), "t")))
    }
  }
}


