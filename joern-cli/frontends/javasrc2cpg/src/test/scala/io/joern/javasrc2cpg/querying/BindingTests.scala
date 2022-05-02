package io.joern.javasrc2cpg.querying

import io.shiftleft.semanticcpg.language._
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture

class BindingTests extends JavaSrcCode2CpgFixture {
  "override generic method" should {
    val cpg = code(
      """
        |import java.util.function.Consumer;
        |
        |class SomeConsumer implements Consumer<Integer> {
        |  @Override
        |  public void accept(Integer i) {}
        |}
        |""".stripMargin
    )
    "have concrete binding" in {
      val typeDecl = cpg.typeDecl(".*SomeConsumer.*").head
      val methodBinding = typeDecl.methodBinding.name("accept")
        .map(binding => (binding.methodFullName, binding.name, binding.signature)).l
      methodBinding should contain theSameElementsAs List(
        ("SomeConsumer.accept:void(java.lang.Integer)", "accept", "void(java.lang.Integer)"),
        ("SomeConsumer.accept:void(java.lang.Object)", "accept", "void(java.lang.Object)")
      )
    }
  }
}
