package org.jetbrains.plugins.scala
package format.scalafmt

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.time.LocalDateTime
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.actor.Props
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.config.Docstrings.JavaDoc
import org.scalafmt.config.LineEndings
import org.scalafmt.config.ScalafmtConfig

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.io.BufferedSource
import scala.io.Codec
import scala.io.Source
import scala.util.control.NonFatal
import scala.util.matching.Regex

object Formatter {

  case class Settings(excludes: Set[Regex], includes: Set[Regex])

  object Settings {
    def empty = Settings(Set.empty, Set.empty)
  }

  def readSettings(configFile: File): Settings =
    try {
      val config = ConfigFactory.parseFile(configFile)
      def get(name: String) = config.getStringList(name).asScala.map(_.r).toSet
      Settings(get("excludes"), get("includes"))
    } catch {
      case NonFatal(e) =>
        println(s"WARNING: Could not read or parse the formatter config file due to $e, assuming empty.")
        Settings.empty
    }

  def shouldBeFormatted(path: String, settings: Settings): Boolean = {
    def matches(r: Regex): Boolean = r.pattern.matcher(path).matches()

    !settings.excludes.exists(matches) && settings.includes.exists(matches)
  }

  private val defaultStyle: ScalafmtConfig = {
    val default = ScalafmtConfig.default
    default.copy(
      maxColumn = 120,
      docstrings = JavaDoc,
      align = default.align.copy(
        openParenCallSite = false,
        openParenDefnSite = false
      ),
      continuationIndent = default.continuationIndent.copy(
        defnSite = 4
      ),
      assumeStandardLibraryStripMargin = true,
      encoding = Codec.UTF8,
      lineEndings = LineEndings.preserve
    )
  }

  def format(source: String, isSbt: Boolean = false): String = {
    val style =
      if (isSbt) defaultStyle.withDialect(scala.meta.dialects.Sbt0137)
      else defaultStyle
    Scalafmt.format(
      source,
      style
    ) match {
      case Formatted.Failure(e) =>
        throw e
      case Formatted.Success(formatted) =>
        formatted
    }
  }

  private def formatFiles(files: List[Path], start: LocalDateTime, maxConsoleLength: Int): Unit = {
    val actorSystem = ActorSystem("formatter")
    val workerCount = Runtime.getRuntime.availableProcessors()
    val master = actorSystem.actorOf(Props(classOf[Collector], workerCount, start, maxConsoleLength))
    import akka.pattern.ask
    implicit val timeout: Timeout = Timeout(10, TimeUnit.MINUTES)
    val result = master ? FormatFiles(files)
    Await.ready(result, timeout.duration)
    actorSystem.terminate()
  }

  private def saveChunkedDiff(source: BufferedSource, sourcesChunkedFile: Path): Unit = {
    val chunked = source
      .getLines()
      .grouped(50)
      .map { _.mkString(" ") }
      .mkString("\n")
    Files.write(sourcesChunkedFile, chunked.getBytes(StandardCharsets.UTF_8))
  }
}
