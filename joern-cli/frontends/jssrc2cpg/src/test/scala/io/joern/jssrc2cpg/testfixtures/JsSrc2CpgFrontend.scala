package io.joern.jssrc2cpg.testfixtures

import better.files.File
import io.joern.jssrc2cpg.JsSrc2Cpg
import io.joern.jssrc2cpg.Config
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.testfixtures.{CodeToCpgFixture, LanguageFrontend}
import org.scalatest.Inside

class JsSrc2CpgFrontend(override val fileSuffix: String = ".js") extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOut      = File(sourceCodePath.getAbsolutePath) / "cpg.bin"
    val packageJson = File(sourceCodePath.getAbsolutePath) / "package.json"
    packageJson.createIfNotExists()
    packageJson.deleteOnExit()
    cpgOut.deleteOnExit()
    val jssrc2cpg = new JsSrc2Cpg()
    val config    = Config(inputPaths = Set(sourceCodePath.getAbsolutePath), outputPath = cpgOut.toString())
    jssrc2cpg.createCpg(config).get
  }
}

class JsSrc2CpgSuite(fileSuffix: String = ".js") extends CodeToCpgFixture(new JsSrc2CpgFrontend(fileSuffix)) with Inside