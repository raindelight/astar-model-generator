package debugger

import com.beepboop.app.astar.ModelNodeTMP
import com.beepboop.app.dataprovider.{AStarSnapshot, PersistenceManager}

import java.awt.{BorderLayout, Dimension, FlowLayout}
import javax.swing.*
import javax.swing.tree.*
import javax.swing.table.DefaultTableModel
import java.awt.event.{ActionListener}
import java.io.*
import scala.util.{Failure, Success, Try}
import scala.reflect.ClassTag
import com.beepboop.app.dataprovider.DataProvider


object VisualDebugger {
  def launch(initialFilePath: String): Unit = {
    System.setProperty("sun.java2d.xrender", "false")
    SwingUtilities.invokeLater(() => {
      try {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
      } catch {
        case _: Exception =>
      }
      val frame = new JFrame("A* Visual Debugger")
      frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      frame.setSize(1300, 900)
      val panel = new DebuggerPanel()
      frame.add(panel)
      if (new File(initialFilePath).exists()) panel.loadFile(initialFilePath)
      frame.setVisible(true)
    })
  }
}

class DebuggerPanel extends JPanel(new BorderLayout()) {

  private var currentSnapshot: Option[AStarSnapshot] = None
  private val objectCache = scala.collection.mutable.Map[String, Any]()
  private var currentLoadedContext = Map[String, Any]()

  private val listModel = new DefaultListModel[AnyRef]()
  private val nodeList = new JList[AnyRef](listModel)
  private val openSetRadio = new JRadioButton("Open Set", true)
  private val visitedSetRadio = new JRadioButton("Visited Set")

  private val treeRoot = new DefaultMutableTreeNode("Select a Node")
  private val treeModel = new DefaultTreeModel(treeRoot)
  private val expressionTree = new JTree(treeModel)

  private val contextTableModel = new DefaultTableModel(Array[Object]("Variable Name", "Value"), 0) {
    override def isCellEditable(row: Int, column: Int): Boolean = column == 1
  }
  private val contextTable = new JTable(contextTableModel)

  private val solutionIndexSpinner = new JSpinner(new SpinnerNumberModel(0, 0, 999999, 1))
  private val loadSolutionButton = new JButton("Load Solution Context")
  private val evalButton = new JButton("Re-Evaluate Tree")
  private val expandAllButton = new JButton("Expand All")
  private val statusLabel = new JLabel("Ready")

  initLayout()
  initListeners()

  private def initLayout(): Unit = {
    val leftPanel = new JPanel(new BorderLayout())
    val radioPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val group = new ButtonGroup();
    group.add(openSetRadio);
    group.add(visitedSetRadio)
    radioPanel.add(openSetRadio);
    radioPanel.add(visitedSetRadio)
    leftPanel.add(radioPanel, BorderLayout.NORTH)
    leftPanel.add(new JScrollPane(nodeList), BorderLayout.CENTER)
    leftPanel.setPreferredSize(new Dimension(350, 0))

    val treePanel = new JPanel(new BorderLayout())
    treePanel.setBorder(BorderFactory.createTitledBorder("Combined Expression Tree"))
    treePanel.add(new JScrollPane(expressionTree), BorderLayout.CENTER)

    val bottomPanel = new JPanel(new BorderLayout())
    bottomPanel.setBorder(BorderFactory.createTitledBorder("Evaluation Context"))

    val tableScroll = new JScrollPane(contextTable)
    tableScroll.setPreferredSize(new Dimension(0, 150))

    val controls = new JPanel(new FlowLayout(FlowLayout.LEFT))
    controls.add(new JLabel("Solution Index:"))
    controls.add(solutionIndexSpinner)
    controls.add(loadSolutionButton)
    controls.add(Box.createHorizontalStrut(20))
    controls.add(expandAllButton)
    controls.add(evalButton)
    controls.add(Box.createHorizontalStrut(10))
    controls.add(statusLabel)

    bottomPanel.add(tableScroll, BorderLayout.CENTER)
    bottomPanel.add(controls, BorderLayout.SOUTH)

    val rightSplit = new JSplitPane(JSplitPane.VERTICAL_SPLIT, treePanel, bottomPanel)
    rightSplit.setResizeWeight(0.7)

    add(new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanel, rightSplit), BorderLayout.CENTER)
  }

  private def initListeners(): Unit = {
    val radioListener: ActionListener = _ => refreshNodeList()
    openSetRadio.addActionListener(radioListener)
    visitedSetRadio.addActionListener(radioListener)

    nodeList.addListSelectionListener(e => {
      if (!e.getValueIsAdjusting && nodeList.getSelectedValue != null) {
        extractAndShowExpression(nodeList.getSelectedValue)
      }
    })

    loadSolutionButton.addActionListener(_ => {
      val idx = solutionIndexSpinner.getValue.asInstanceOf[Int]
      loadContextFromDataProvider(idx)
    })

    evalButton.addActionListener(_ => performFullTreeEvaluation())

    expandAllButton.addActionListener(_ => expandAll(expressionTree))
  }


  private def loadContextFromDataProvider(index: Int): Unit = {
    try {
      val ctx = DataProvider.getSolutionContext(index)
      currentLoadedContext = ctx

      objectCache.clear()
      contextTableModel.setRowCount(0)

      ctx.keys.toList.sorted.foreach { key =>
        val value = ctx(key)
        objectCache.put(key, value)
        contextTableModel.addRow(Array[Object](key, value.toString))
      }

      statusLabel.setText(s"Loaded solution #$index")
      if (treeRoot.getChildCount > 0) performFullTreeEvaluation()

    } catch {
      case e: Exception =>
        JOptionPane.showMessageDialog(this, s"Failed to load solution #$index:\n${e.getMessage}")
    }
  }

  private def extractAndShowExpression(nodeWrapper: AnyRef): Unit = {
    try {
      val method = nodeWrapper.getClass.getMethod("constraint")
      val expr = method.invoke(nodeWrapper)

      treeRoot.removeAllChildren()
      treeRoot.setUserObject(new NodeWrapper("Root", expr))
      buildTreeRecursive(treeRoot, expr)
      treeModel.reload()

      expandAll(expressionTree)

      if (currentLoadedContext.nonEmpty) {
        performFullTreeEvaluation()
      } else {
        loadContextFromDataProvider(0)
      }

    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

  private def buildTreeRecursive(parentUi: DefaultMutableTreeNode, obj: Any): Unit = {
    obj match {
      case p: Product =>
        p.productIterator.zip(p.productElementNames).foreach { case (child, name) =>
          if (isExpression(child)) {
            val node = new DefaultMutableTreeNode(new NodeWrapper(name, child))
            parentUi.add(node)
            buildTreeRecursive(node, child)
          } else if (child.isInstanceOf[List[_]]) {
            child.asInstanceOf[List[_]].zipWithIndex.foreach { case (item, idx) =>
              if (isExpression(item)) {
                val node = new DefaultMutableTreeNode(new NodeWrapper(s"$name[$idx]", item))
                parentUi.add(node)
                buildTreeRecursive(node, item)
              }
            }
          }
        }
      case _ =>
    }
  }


  private def performFullTreeEvaluation(): Unit = {
    val evalCtx = buildContextFromTable()
    val nodeEnum = treeRoot.breadthFirstEnumeration()

    while (nodeEnum.hasMoreElements) {
      val uiNode = nodeEnum.nextElement().asInstanceOf[DefaultMutableTreeNode]

      uiNode.getUserObject match {
        case wrapper: NodeWrapper =>
          wrapper.evalResult = try {
            val m = wrapper.obj.getClass.getMethod("eval", classOf[Map[String, Any]])
            val res = m.invoke(wrapper.obj, evalCtx)
            res match {
              case b: Boolean => if (b) "true" else "false"
              case other => other.toString
            }
          } catch {
            case _: Exception => "?"
          }

          wrapper.distResult = try {
            val m = wrapper.obj.getClass.getMethod("distance", classOf[Map[String, Any]])
            val res = m.invoke(wrapper.obj, evalCtx)
            res.toString
          } catch {
            case _: Exception => "?"
          }

        case _ =>
      }
    }

    treeModel.reload()
    statusLabel.setText("Updated Eval and Distance.")
  }

  private def buildContextFromTable(): Map[String, Any] = {
    val ctx = scala.collection.mutable.Map[String, Any]()
    for (i <- 0 until contextTableModel.getRowCount) {
      val key = contextTableModel.getValueAt(i, 0).toString
      val tableVal = contextTableModel.getValueAt(i, 1).toString.trim

      if (objectCache.contains(key) && objectCache(key).toString == tableVal) {
        ctx.put(key, objectCache(key))
      } else if (tableVal.nonEmpty) {
        val pV = if (Try(tableVal.toInt).isSuccess) tableVal.toInt
        else if (Try(tableVal.toDouble).isSuccess) tableVal.toDouble
        else if (tableVal == "true") true else if (tableVal == "false") false
        else tableVal
        ctx.put(key, pV)
      }
    }
    ctx.toMap
  }


  def loadFile(path: String): Unit = {
    PersistenceManager.loadAStarState(path) match {
      case Success(snap) =>
        currentSnapshot = Some(snap)
        refreshNodeList()
      case Failure(e) => JOptionPane.showMessageDialog(this, e.getMessage)
    }
  }

  private def refreshNodeList(): Unit = {
    listModel.clear()
    currentSnapshot.foreach { snap =>
      val items = if (openSetRadio.isSelected) snap.openSetItems else snap.visitedItems
      items.foreach(listModel.addElement)
    }
  }

  private def isExpression(o: Any): Boolean = {
    if (o == null) return false
    var c = o.getClass
    while (c != null) {
      if (c.getName.contains("Expression")) return true
      c = c.getSuperclass
    }
    false
  }

  private def expandAll(tree: JTree): Unit = {
    for (i <- 0 until tree.getRowCount) tree.expandRow(i)
    var i = 0
    while (i < tree.getRowCount) {
      tree.expandRow(i)
      i += 1
    }
  }

  class NodeWrapper(val field: String, val obj: Any) {
    var evalResult: String = "?"
    var distResult: String = "?"

    override def toString: String = {
      val typeName = obj.getClass.getSimpleName
      val content = obj.toString

      val base = s"$field: $typeName ($content)"

      if (evalResult != "?" || distResult != "?") {
        s"<html>$base <font color='blue'><b> ➔ Eval: $evalResult | Dist: $distResult</b></font></html>"
      } else {
        base
      }
    }
  }
}