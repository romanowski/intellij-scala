package org.jetbrains.plugins.scala
package format.scalafmt

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.time.Duration
import java.time.LocalDateTime

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.routing.RoundRobinPool

import scala.io.Source
import scala.util.control.NonFatal

sealed trait FormattingResult
object FormattingSuccess extends FormattingResult
object FormattingSkipped extends FormattingResult
case class FormattingFailed(path: Path, error: Throwable) extends FormattingResult

case class FormatFiles(paths: Seq[Path])
case class FormatFile(path: Path)
case class FileFormatted(path: Path, result: FormattingResult)

/**
 * Collector actor, gets the messages to/from the workers, takes care of the in-place message updates and shuts down
 * when all files are formatted.
 */
class Collector(workerCount: Int, startTime: LocalDateTime, maxConsoleLength: Int) extends Actor {
  var formattedCount: Int = 0
  var skippedCount: Int = 0
  var failures: List[FormattingFailed] = List.empty
  var fileCount: Int = 0
  var longestLine: Int = 1

  var originalSender: ActorRef = _
  var totalFileCount: Int = _

  def waitingForWork: Receive = {
    case FormatFiles(paths) =>
      originalSender = sender()
      totalFileCount = paths.size
      val worker: ActorRef = context.actorOf(RoundRobinPool(workerCount).props(Props[Worker]), "workers")
      paths.foreach { f =>
        worker ! FormatFile(f)
      }
      context.become(working)
  }

  def working: Receive = {
    case FileFormatted(path, result) =>
      result match {
        case FormattingSuccess         => formattedCount += 1
        case FormattingSkipped         => skippedCount += 1
        case failure: FormattingFailed => failures ::= failure
      }
      fileCount += 1
      // If the message is longer than console buffer width the \r trick does not work in Windows cmd.
      val msg = trimmedMsg(path, maxConsoleLength)
      Console.print("\r" + (" " * longestLine))
      Console.print("\r" + msg)
      Console.flush()
      longestLine = scala.math.max(longestLine, msg.length)

      if (fileCount == totalFileCount) {
        printSummary()
        originalSender ! "Finished!"
        context.stop(self)
      }
  }

  override def receive: Receive = waitingForWork

  private def trimmedMsg(path: Path, maxLength: Int): String = {
    val msgPrefix = s"[$fileCount/$totalFileCount] "

    val bufferForCarriageReturn = 1
    val maxPathLength = maxLength - msgPrefix.length - bufferForCarriageReturn
    val pathString = path.toString.drop(2) // we can drop the './' from the beginning
    val trimmedPathString = {
      val ellipsis = "..."
      if (pathString.length > maxPathLength)
        ellipsis + pathString.substring(pathString.length - maxPathLength + ellipsis.length)
      else pathString
    }

    s"$msgPrefix$trimmedPathString"
  }

  private def printSummary(): Unit = {
    println(s"\nFormatted: $formattedCount, Skipped: $skippedCount, Total: $totalFileCount.")
    if (failures.nonEmpty) {
      println(s"Failed to format ${failures.size} file(s):")
      failures.foreach { failure =>
        println(s" - ${failure.path} : ${failure.error.getClass.getName} ${failure.error.getMessage}")
      }
    }
    val endTime = LocalDateTime.now()
    val duration = Duration.between(startTime, endTime).getSeconds
    println(s"Took: $duration.")
  }
}

/**
 * Worker actor, formats the file and sends the notification to the collector actor
 */
class Worker extends Actor {
  override def receive: Receive = {
    case FormatFile(path) =>
      val result = try {
        val text = Source.fromFile(path.toFile).mkString
        val newText = StratoFormatter.format(text)
        if (text != newText) {
          Files.write(path, newText.getBytes(StandardCharsets.UTF_8))
          FormattingSuccess
        } else {
          FormattingSkipped
        }
      } catch {
        case NonFatal(e) =>
          FormattingFailed(path, e)
      }
      sender() ! FileFormatted(path, result)
  }
}
