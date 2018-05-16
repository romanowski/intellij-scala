package org.jetbrains.plugins.scala
package format.scalafmt

import java.util

import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiFile
import com.intellij.psi.codeStyle.ChangedRangesInfo
import com.intellij.psi.impl.source.codeStyle.CodeStyleManagerImpl
import com.ms.stratosphere.jetfire.commands.formatter.ScalaFmtFormatter

class ScalaFmtCodeStyleManagerImpl(project: Project) extends CodeStyleManagerImpl(project) {

  override def reformatText(file: PsiFile, ranges: util.Collection[TextRange], editor: Editor): Unit = {
    formatUsingScalaFmt(file, super.reformatText(file, ranges, editor))
  }

  override def reformatTextWithContext(file: PsiFile, info: ChangedRangesInfo): Unit = {
   formatUsingScalaFmt(file, super.reformatTextWithContext(file, info))
  }

  private def formatUsingScalaFmt(file: PsiFile, fallbackFormatter: => Unit): Unit = {
    ScalaFmtFormatter.forFile(file, fallbackFormatter).foreach(_.format())
  }

}
