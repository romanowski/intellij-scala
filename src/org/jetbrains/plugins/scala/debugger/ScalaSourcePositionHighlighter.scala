package org.jetbrains.plugins.scala.debugger

import com.intellij.debugger.SourcePosition
import com.intellij.debugger.engine.SourcePositionHighlighter
import com.intellij.openapi.util.TextRange
import org.jetbrains.plugins.scala.debugger.evaluation.util.DebuggerUtil

/**
 * @author Nikolay.Tropin
 */
class ScalaSourcePositionHighlighter extends SourcePositionHighlighter {
  override def getHighlightRange(sourcePosition: SourcePosition): TextRange = {
    Option(sourcePosition.getElementAt).flatMap(DebuggerUtil.getContainingMethod).map(_.getTextRange).orNull
  }
}
