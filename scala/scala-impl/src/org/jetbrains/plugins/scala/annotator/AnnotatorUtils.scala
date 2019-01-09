package org.jetbrains.plugins.scala
package annotator

import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.codeInspection.ProblemHighlightType
import com.intellij.lang.annotation.{Annotation, AnnotationHolder}
import com.intellij.openapi.editor.markup.TextAttributes
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.annotator.quickfix.ReportHighlightingErrorQuickFix
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlockExpr, ScExpression}
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, ScTypeExt}
import org.jetbrains.plugins.scala.lang.psi.types.api.ScTypePresentation
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult
import org.jetbrains.plugins.scala.project.ProjectContext

/**
 * @author Aleksander Podkhalyuzin
 * Date: 25.03.2009
 */

// TODO move to org.jetbrains.plugins.scala.lang.psi.annotator
object AnnotatorUtils {
  def proccessError(error: String, element: PsiElement, holder: AnnotationHolder, fixes: IntentionAction*) {
    proccessError(error, element.getTextRange, holder, fixes: _*)
  }

  def proccessError(error: String, range: TextRange, holder: AnnotationHolder, fixes: IntentionAction*) {
    val annotation = holder.createErrorAnnotation(range, error)
    for (fix <- fixes) annotation.registerFix(fix)
  }

  def proccessWarning(error: String, element: PsiElement, holder: AnnotationHolder, fixes: IntentionAction*) {
    proccessWarning(error, element.getTextRange, holder, fixes: _*)
  }

  def proccessWarning(error: String, range: TextRange, holder: AnnotationHolder, fixes: IntentionAction*) {
    val annotation: Annotation = holder.createWarningAnnotation(range, error)
    for (fix <- fixes) annotation.registerFix(fix)
  }

  def checkConformance(expression: ScExpression, typeElement: ScTypeElement, holder: AnnotationHolder) {
    implicit val ctx: ProjectContext = expression

    expression.getTypeAfterImplicitConversion().tr.foreach {actual =>
      val expected = typeElement.calcType
      if (!actual.conforms(expected)) {
        val expr = expression match {
          case b: ScBlockExpr => b.getRBrace.map(_.getPsi).getOrElse(b)
          case _ => expression
        }
        val (actualText, expText) = ScTypePresentation.different(actual, expected)
        val annotation = holder.createErrorAnnotation(expr,
          ScalaBundle.message("type.mismatch.found.required", actualText, expText))
        annotation.registerFix(ReportHighlightingErrorQuickFix)
      }
    }
  }

  def registerTypeMismatchError(actualType: ScType, expectedType: ScType, holder: AnnotationHolder, expression: ScExpression): Unit = {
    //TODO show parameter name
    if (!actualType.conforms(expectedType)) {
      val (expectedText, actualText) = ScTypePresentation.different(expectedType, actualType)
      val message = ScalaBundle.message("type.mismatch.expected.actual", expectedText, actualText)
      val annotation = holder.createErrorAnnotation(expression, message)
      annotation.registerFix(ReportHighlightingErrorQuickFix)
    }
  }

  def annotationWithoutHighlighting(holder: AnnotationHolder, te: PsiElement): Annotation = {
    val teAnnotation = holder.createErrorAnnotation(te, null)
    teAnnotation.setHighlightType(ProblemHighlightType.INFORMATION)
    val emptyAttr = new TextAttributes()
    teAnnotation.setEnforcedTextAttributes(emptyAttr)
    teAnnotation
  }

  /**
    * This method will return checked conformance if it's possible to check it.
    * In other way it will return true to avoid red code.
    * Check conformance in case l = r.
    */
  def smartCheckConformance(l: TypeResult, r: TypeResult): Boolean = {
    val leftType = l match {
      case Right(res) => res
      case _ => return true
    }
    val rightType = r match {
      case Right(res) => res
      case _ => return true
    }
    rightType.conforms(leftType)
  }
}