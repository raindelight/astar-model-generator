package com.beepboop.app.dataprovider

import com.beepboop.app.astar.ModelNodeTMP
import com.beepboop.app.logger.LogTrait
import java.io._
import scala.util.Try

case class AStarSnapshot(
                          openSetItems: List[ModelNodeTMP],
                          visitedItems: Set[ModelNodeTMP]
                        ) extends Serializable

object PersistenceManager extends LogTrait {

  def saveConstraintsToCSV(nodes: Iterable[ModelNodeTMP], filename: String): Unit = {
    val file = new File(filename)
    warn(s"DEBUG: Attempting to create CSV at absolute path: ${file.getAbsolutePath}")
    val bw = new BufferedWriter(new FileWriter(file))
    try {
      bw.write("Constraint_String|Depth_g|Heuristic_h|Score_f\n")

      nodes.foreach { node =>
        val cleanStr = node.constraint.toString.replaceAll("[\r\n]+", " ")
        bw.write(s"$cleanStr|${node.g}|${node.h}|${node.f}\n")
      }
      info(s"Saved ${nodes.size} solutions to CSV: $filename")
    } catch {
      case e: Exception => error(s"Error writing CSV: ${e.getMessage}")
    } finally {
      bw.close()
    }
  }

  def saveAStarState(snapshot: AStarSnapshot, filename: String): Unit = {
    useObjectStream(filename) { out =>
      out.writeObject(snapshot)
    }
  }

  def loadAStarState(filename: String): Try[AStarSnapshot] = {
    Try {
      val fileIn = new FileInputStream(filename)
      val in = new ObjectInputStream(fileIn)
      try {
        in.readObject().asInstanceOf[AStarSnapshot]
      } finally {
        in.close()
        fileIn.close()
      }
    }
  }

  private def useObjectStream(filename: String)(op: ObjectOutput => Unit): Unit = {
    try {
      val fileOut = new FileOutputStream(filename)
      val out = new ObjectOutputStream(fileOut)
      op(out)
      out.close()
      fileOut.close()
      info(s"Successfully saved binary state to $filename")
    } catch {
      case e: Exception => error(s"Failed to save binary state: ${e.getMessage}")
    }
  }
}