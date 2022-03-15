package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewMetaData
import io.shiftleft.passes.SimpleCpgPass
import org.slf4j.LoggerFactory

class JsMetaDataPass(cpg: Cpg, hash: String) extends SimpleCpgPass(cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    logger.debug("Generating meta-data.")
    val metaNode = NewMetaData().language("NEWJS").hash(hash).version("0.1")
    diffGraph.addNode(metaNode)
  }

}