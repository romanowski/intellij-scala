package org.jetbrains.sbt
package project.module

import java.awt.event.{ActionEvent, ActionListener}
import java.util
import javax.swing.JPanel
import javax.swing.table.AbstractTableModel

import com.intellij.openapi.project.{Project, ProjectManager}
import com.intellij.openapi.roots.ui.configuration.{ModuleConfigurationState, ModuleElementsEditor}
import com.intellij.ui.CollectionListModel
import com.intellij.util.text.DateFormatUtil
import org.jetbrains.plugins.scala.util.JListCompatibility
import org.jetbrains.plugins.scala.util.JListCompatibility.CollectionListModelWrapper
import org.jetbrains.sbt.resolvers.{SbtResolver, SbtResolversManager}
import org.jetbrains.sbt.resolvers.indexes.ResolverIndex
import org.jetbrains.sbt.resolvers.SbtResolversManager
import org.jetbrains.sbt.settings.SbtSystemSettings

import scala.collection.JavaConverters._

/**
 * @author Nikolay Obedin
 * @since 12/1/14.
 */
class SbtModuleSettingsEditor (state: ModuleConfigurationState) extends ModuleElementsEditor(state) {
  private val myForm = new SbtModuleSettingsForm
  private val modelWrapper = new CollectionListModelWrapper(new CollectionListModel[String](new util.ArrayList[String]))
  private val resolvers = SbtModule.getResolversFrom(getModel.getModule).toSeq

  def getDisplayName = SbtBundle("sbt.settings.sbtModuleSettings")

  def getHelpTopic = null

  def saveData() {}

  def createComponentImpl(): JPanel = {
    myForm.sbtImportsList.setEmptyText(SbtBundle("sbt.settings.noImplicitImportsFound"))
    JListCompatibility.setModel(myForm.sbtImportsList, modelWrapper.getModelRaw)

    myForm.updateButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val resolversToUpdate: Seq[SbtResolver] = myForm.resolversTable.getSelectedRows map (resolvers(_))
        SbtResolversManager.getInstance(state.getProject).updateWithProgress(resolversToUpdate)
      }
    })

    myForm.mainPanel
  }

  override def reset() {
    val moduleSettings = SbtSystemSettings.getInstance(state.getProject).getLinkedProjectSettings(getModel.getModule)
    myForm.sbtVersionTextField.setText(moduleSettings.map(_.sbtVersion).getOrElse(SbtBundle("sbt.settings.sbtVersionNotDetected")))

    modelWrapper.getModel.replaceAll(SbtModule.getImportsFrom(getModel.getModule).asJava)

    myForm.resolversTable.setModel(new ResolversModel(resolvers, state.getProject))
    myForm.resolversTable.setRowSelectionInterval(0, 0)
    myForm.resolversTable.getColumnModel.getColumn(0).setPreferredWidth(50)
    myForm.resolversTable.getColumnModel.getColumn(1).setPreferredWidth(400)
    myForm.resolversTable.getColumnModel.getColumn(2).setPreferredWidth(30)
  }
}

private class ResolversModel(val resolvers: Seq[SbtResolver], val project:Project) extends AbstractTableModel {

  private val columns = Seq(
    SbtBundle("sbt.settings.resolvers.name"),
    SbtBundle("sbt.settings.resolvers.url"),
    SbtBundle("sbt.settings.resolvers.updated")
  )

  def getColumnCount: Int = columns.size

  def getRowCount: Int = resolvers.size

  override def getColumnName(columnIndex: Int): String = columns(columnIndex)

  def getValueAt(rowIndex: Int, columnIndex: Int): String = columnIndex match {
    case 0 => resolvers(rowIndex).name
    case 1 => resolvers(rowIndex).root
    case 2 =>
      val ts: Long = resolvers(rowIndex).getIndex.getUpdateTimeStamp(project)
      if (ts == ResolverIndex.NO_TIMESTAMP)
        SbtBundle("sbt.settings.resolvers.neverUpdated")
      else
        DateFormatUtil.formatDate(ts)
  }
}
