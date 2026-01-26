package com.beepboop.app.cpicker

import com.beepboop.app.components.*
import com.beepboop.app.utils.AppConfig

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import java.io.File


object ConstraintSaver {
  var config: AppConfig = AppConfig.get

  def save(constraints: Expression[?]*): Path = {
    val tempPath: Path = Files.createTempFile("mzn_temp_", ".mzn")
    tempPath.toFile.deleteOnExit();

    val content = new StringBuilder()

    content.append(s"include \"${config.modelPath}\";\n")
    content.append(s"include \"alldifferent.mzn\";\n\n")
    constraints.foreach { c =>
      content.append(s"constraint ${c.toString};\n")
    }
    Files.write(
      tempPath,
      content.toString().getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE,
      StandardOpenOption.TRUNCATE_EXISTING,
      StandardOpenOption.SYNC
    )
    tempPath
  }

}
