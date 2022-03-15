package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._

import scala.jdk.CollectionConverters._

class JsMetaDataPassTest extends AbstractPassTest {

  "MetaDataPass" should {
    val cpg = Cpg.emptyCpg
    new JsMetaDataPass(cpg, "somehash").createAndApply()

    "create exactly 1 node" in {
      cpg.graph.V.asScala.size shouldBe 1
    }

    "create no edges" in {
      cpg.graph.E.asScala.size shouldBe 0
    }

    "create a metadata node with correct language" in {
      cpg.metaData.language.l shouldBe List("NEWJS")
    }

    "create a metadata node with a hash" in {
      cpg.metaData.hash.l shouldBe List("somehash")
    }
  }

}
