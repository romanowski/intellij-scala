package org.jetbrains.plugins.scala
package format.scalafmt

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.editor.Document
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi._

import scala.util.Failure
import scala.util.Success

object ScalaFmtFormatter {

  def forFile(psiFile: PsiFile, fallbackFormatter: => Unit): Option[ScalaFmtFormatter] = {
    Option(psiFile).map(new ScalaFmtPsiFileFormatter(_, fallbackFormatter))
  }
}

class ScalaFmtPsiFileFormatter(psiFile: PsiFile, fallbackFormatter: => Unit)
  extends ScalaFmtFormatter {

  override def format(): Unit = {
    val document = psiFile.getViewProvider.getDocument
    val file = FileDocumentManager.getInstance().getFile(document)
    if (shouldUseScalaFmt(file)) {
      formatSingleFile(file, document)
    } else {
      fallbackFormatter
    }
  }
}

/**
 * Base for scalafmt formatters.
 */
abstract class ScalaFmtFormatter {

  private val notifier = ???
  private val configLocation: File = ???
  private val config = Formatter.readSettings(configLocation)

  def format(): Unit

  protected def isSbt(virtualFile: VirtualFile): Boolean = virtualFile.getFileType.getName == "SBT"

  protected def shouldUseScalaFmt(virtualFile: VirtualFile): Boolean = {
    val isScala: Boolean = virtualFile.getFileType.getName == "Scala"

    val isScalaFile: Boolean = isScala || isSbt(virtualFile)

    val useScalaFmtForThisFile = Formatter.shouldBeFormatted(virtualFile.getPath, config)

    isScalaFile && useScalaFmtForThisFile
  }

  protected def formatSingleFile(virtualFile: VirtualFile, document: Document): Unit = {
    val source = document.getText()

    runAsych { indicator =>  // This use our internal lib
      Formatter.format(source, isSbt(virtualFile))
    }.onComplete {
      case Success(formatted) if source != formatted =>
        updateDocumentText(document, formatted)
      case Success(_) => // no changes, do nothing
      case Failure(e) =>
        // notifier.error("Formatting failed", s"Could not format ${ virtualFile.getName } due to $e!", project = null)
    }
  }

  private def updateDocumentText(document: Document, text: String): Unit = {
    ApplicationManager.getApplication.runWriteAction(new Runnable {
      override def run(): Unit =
        CommandProcessor
          .getInstance()
          .runUndoTransparentAction(() => document.setText(text))
    })
  }
}