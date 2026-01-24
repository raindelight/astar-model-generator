package com.beepboop.app.dataprovider

import com.beepboop.app.astar.ModelNodeTMP
import com.beepboop.app.components.Expression
import com.beepboop.app.cpicker.ConstraintData
import com.beepboop.app.logger.{LogTrait, Profiler}

import java.io.*
import scala.annotation.targetName
import scala.util.{Failure, Success, Try}

case class AStarSnapshot(
                          openSetItems: List[ModelNodeTMP],
                          visitedItems: Set[ModelNodeTMP]
                        ) extends Serializable

object PersistenceManager extends LogTrait {

  @targetName("ConstraintData")
  def saveConstraintsToCSV(cd: Iterable[ConstraintData], filename: String): Unit = {
    val file = new File(filename)
    Option(file.getParentFile).foreach(_.mkdirs())
    val bw = new BufferedWriter(new FileWriter(file))
    try {
      bw.write("Constraint_String|Score_f|Sol_count\n")

      cd.foreach { c =>
        val cleanStr = c.constraint.toString.replaceAll("[\r\n]+", " ")
        bw.write(s"$cleanStr|${c.heuristics}|${c.solCount}\n")
      }
      info(s"Saved ${cd.size} solutions to CSV: $filename")
    } catch {
      case e: Exception => error(s"Error writing CSV: ${e.getMessage}")
    } finally {
      bw.close()
    }

  }

  @targetName("ModelNodeTMP")
  def saveConstraintsToCSV(nodes: Iterable[ModelNodeTMP], filename: String): Unit = {
    val file = new File(filename)
    Option(file.getParentFile).foreach(_.mkdirs())
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
    val file = new File(filename)
    Option(file.getParentFile).foreach(_.mkdirs())
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

  def saveCheckpoint(snapshot: AStarSnapshot, checkpointPath: String, csvPath: String): Unit = {
    saveAStarState(snapshot, checkpointPath)
    if (snapshot.visitedItems.nonEmpty) {
      saveConstraintsToCSV(snapshot.visitedItems, csvPath)
    }
  }

  def performEmergencyBackup(snapshot: AStarSnapshot, checkpointPath: String, csvPath: String): Unit = {
    try {
      scala.Console.err.println("\n!!! CAUGHT EXIT SIGNAL (Ctrl+C) !!!")
      scala.Console.err.println("Attempting emergency backup...")
      saveCheckpoint(snapshot, checkpointPath, csvPath)
      scala.Console.err.println("Emergency backup completed successfully.")
      Profiler.report()
    } catch {
      case e: Exception => error(s"Emergency backup failed: ${e.getMessage}")
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
      case e: Exception => scala.Console.err.println(s"Emergency backup failed: ${e.getMessage}")
    }
  }
}