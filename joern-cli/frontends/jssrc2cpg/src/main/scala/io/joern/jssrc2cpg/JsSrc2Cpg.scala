package io.joern.jssrc2cpg

import better.files.File
import io.joern.jssrc2cpg.JsSrc2Cpg.Config
import io.joern.jssrc2cpg.passes.AstCreationPass
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.jssrc2cpg.utils.Environment
import io.joern.jssrc2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.KeyPoolCreator
import io.shiftleft.semanticcpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.{X2Cpg, X2CpgConfig}
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.passes.frontend.TypeNodePass
import org.slf4j.LoggerFactory
import scopt.OParser

import scala.util.control.NonFatal

class JsSrc2Cpg {

  private val report: Report = new Report()

  def runAndOutput(config: Config): Cpg = {
    val keyPool         = KeyPoolCreator.obtain(2, minValue = 101)
    val metaDataKeyPool = new IntervalKeyPool(1, 100)
    val typesKeyPool    = keyPool.head
    val astKeyPool      = keyPool(1)

    val cpg = newEmptyCpg(Some(config.outputPath))

    File.usingTemporaryDirectory("jssrc2cpgOut") { tmpDir =>
      val files: Set[(String, String)] = config.inputPaths.flatMap(i => AstGenRunner.execute(File(i), tmpDir))
      new MetaDataPass(cpg, "NEWJS", Some(metaDataKeyPool)).createAndApply()
      new AstCreationPass(cpg, files, Some(astKeyPool), config, report).createAndApply()
      new TypeNodePass(List.empty, cpg, Some(typesKeyPool)).createAndApply() // TODO: provide types
      report.print()
    }

    cpg
  }

}

object JsSrc2Cpg {

  private val logger = LoggerFactory.getLogger(classOf[JsSrc2Cpg])

  final case class Config(inputPaths: Set[String] = Set.empty, outputPath: String = X2CpgConfig.defaultOutputPath)
      extends X2CpgConfig[Config] {

    override def withAdditionalInputPath(inputPath: String): Config = copy(inputPaths = inputPaths + inputPath)
    override def withOutputPath(x: String): Config                  = copy(outputPath = x)
  }

  private val frontendSpecificOptions = {
    val builder = OParser.builder[Config]
    import builder.programName
    OParser.sequence(programName("jssrc2cpg"))
  }

  def main(args: Array[String]): Unit = {
    X2Cpg.parseCommandLine(args, frontendSpecificOptions, Config()) match {
      case Some(config) if Environment.valid() =>
        try {
          val cpg = new JsSrc2Cpg().runAndOutput(config)
          cpg.close()
        } catch {
          case NonFatal(ex) =>
            logger.error("Failed to generate CPG.", ex)
            System.exit(1)
        }
      case _ =>
        System.exit(1)
    }
  }

}
