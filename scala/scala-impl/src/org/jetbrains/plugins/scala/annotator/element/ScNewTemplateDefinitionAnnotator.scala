package org.jetbrains.plugins.scala
package annotator
package element

import com.intellij.lang.annotation.AnnotationHolder
import org.jetbrains.plugins.scala.annotator.template.{isAbstract, kindOf, superRefs}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScNewTemplateDefinition

object ScNewTemplateDefinitionAnnotator extends ElementAnnotator[ScNewTemplateDefinition] {

  override def annotate(element: ScNewTemplateDefinition, typeAware: Boolean)
                       (implicit holder: AnnotationHolder): Unit = {
    annotateAbstractInstantiation(element)
  }

  // TODO package private
  def annotateAbstractInstantiation(element: ScNewTemplateDefinition)
                                   (implicit holder: AnnotationHolder): Unit = {
    val hasBody = element.extendsBlock.templateBody.isDefined
    val hasEarlyBody = element.extendsBlock.earlyDefinitions.exists(_.members.nonEmpty)

    if (hasEarlyBody || hasBody) return

    superRefs(element) match {
      case (range, clazz) :: Nil if isAbstract(clazz) =>
        val message = ScalaBundle.message("illegal.instantiation", kindOf(clazz), clazz.name)
        holder.createErrorAnnotation(range, message)
      case _ =>
    }
  }
}
