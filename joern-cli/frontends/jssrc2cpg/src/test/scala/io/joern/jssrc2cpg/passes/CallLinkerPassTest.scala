package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class CallLinkerPassTest extends DataFlowCodeToCpgSuite {
  override val code: String = """
   |function sayhi() {
   |  console.log("Hello World!");
   |}
   |
   |sayhi();
   |""".stripMargin

  "CallLinkerPass" should {
    "resolve a call via callIn correctly" in {
      inside(cpg.method("sayhi").l) { case List(m) =>
        m.name shouldBe "sayhi"
        m.fullName should endWith(".js::program:sayhi")
      }
      inside(cpg.method("sayhi").callIn(NoResolve).l) { case List(call) =>
        call.code shouldBe "sayhi()"
      }
    }
  }
}
