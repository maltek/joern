package io.joern.jssrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import ujson.Obj
import ujson.Value

trait AstForStatementsCreator {

  this: AstCreator =>

  protected def createBlockStatementAsts(json: Value): List[Ast] = {
    val blockStmts = json.arr.map(createBabelNodeInfo).sortBy(_.node != BabelAst.FunctionDeclaration).toList
    blockStmts.map {
      case func @ BabelNodeInfo(BabelAst.FunctionDeclaration) =>
        astForFunctionDeclaration(func, shouldCreateAssignmentCall = true, shouldCreateFunctionReference = true)
      case other =>
        astForNode(other.json)
    }
  }

  protected def astForBlockStatement(block: BabelNodeInfo): Ast = {
    val blockNode = createBlockNode(block.code, block.lineNumber, block.columnNumber)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val blockStatementAsts = createBlockStatementAsts(block.json("body"))
    setArgumentIndices(blockStatementAsts)
    localAstParentStack.pop()
    scope.popScope()
    Ast(blockNode).withChildren(blockStatementAsts)
  }

  protected def astForReturnStatement(ret: BabelNodeInfo): Ast = {
    val retNode = createReturnNode(ret)
    safeObj(ret.json, "argument")
      .map { argument =>
        val argAst = astForNode(Obj(argument))
        returnAst(retNode, List(argAst))
      }
      .getOrElse(Ast(retNode))
  }

  private def astForCatchClause(catchClause: BabelNodeInfo): Ast =
    astForNode(catchClause.json("body"))

  protected def astForTryStatement(tryStmt: BabelNodeInfo): Ast = {
    val tryNode = createControlStructureNode(tryStmt, ControlStructureTypes.TRY)

    val bodyAst = astForNode(tryStmt.json("block"))

    val catchAst = safeObj(tryStmt.json, "handler")
      .map { handler =>
        astForCatchClause(createBabelNodeInfo(Obj(handler)))
      }
      .getOrElse(Ast())

    val finalizerAst = safeObj(tryStmt.json, "finalizer")
      .map { finalizer =>
        astForNode(Obj(finalizer))
      }
      .getOrElse(Ast())

    val tryChildren = List(bodyAst, catchAst, finalizerAst)
    setArgumentIndices(tryChildren)
    Ast(tryNode).withChildren(tryChildren)
  }

  def astForIfStatement(ifStmt: BabelNodeInfo): Ast = {
    val ifNode        = createControlStructureNode(ifStmt, ControlStructureTypes.IF)
    val testAst       = astForNode(ifStmt.json("test"))
    val consequentAst = astForNode(ifStmt.json("consequent"))
    val alternateAst = safeObj(ifStmt.json, "alternate")
      .map { alternate =>
        astForNode(Obj(alternate))
      }
      .getOrElse(Ast())
    val ifChildren = List(testAst, consequentAst, alternateAst)
    setArgumentIndices(ifChildren)
    Ast(ifNode)
      .withChild(testAst)
      .withConditionEdge(ifNode, testAst.nodes.head)
      .withChild(consequentAst)
      .withChild(alternateAst)
  }

  protected def astForDoWhileStatement(doWhileStmt: BabelNodeInfo): Ast = {
    val whileNode = createControlStructureNode(doWhileStmt, ControlStructureTypes.DO)
    val testAst   = astForNode(doWhileStmt.json("test"))
    val bodyAst   = astForNode(doWhileStmt.json("body"))
    setArgumentIndices(List(bodyAst, testAst))
    Ast(whileNode).withChild(bodyAst).withChild(testAst).withConditionEdge(whileNode, testAst.nodes.head)
  }

  protected def astForWhileStatement(whileStmt: BabelNodeInfo): Ast = {
    val whileNode = createControlStructureNode(whileStmt, ControlStructureTypes.WHILE)
    val testAst   = astForNode(whileStmt.json("test"))
    val bodyAst   = astForNode(whileStmt.json("body"))
    setArgumentIndices(List(testAst, bodyAst))
    Ast(whileNode).withChild(testAst).withConditionEdge(whileNode, testAst.nodes.head).withChild(bodyAst)
  }

  protected def astForForStatement(forStmt: BabelNodeInfo): Ast = {
    val forNode = createControlStructureNode(forStmt, ControlStructureTypes.FOR)
    val initAst = safeObj(forStmt.json, "init")
      .map { init =>
        astForNode(Obj(init))
      }
      .getOrElse(Ast())
    val testAst = safeObj(forStmt.json, "test")
      .map { test =>
        astForNode(Obj(test))
      }
      .getOrElse(
        Ast(createLiteralNode("true", Some(Defines.BOOLEAN.label), forStmt.lineNumber, forStmt.columnNumber).order(2))
      )
    val updateAst = safeObj(forStmt.json, "update")
      .map { update =>
        astForNode(Obj(update))
      }
      .getOrElse(Ast())
    val bodyAst = astForNode(forStmt.json("body"))
    setArgumentIndices(List(initAst, testAst, updateAst, bodyAst))
    Ast(forNode).withChild(initAst).withChild(testAst).withChild(updateAst).withChild(bodyAst)
  }

  protected def astForBreakStatement(breakStmt: BabelNodeInfo): Ast =
    Ast(createControlStructureNode(breakStmt, ControlStructureTypes.BREAK))

  protected def astForThrowStatement(throwStmt: BabelNodeInfo): Ast = {
    val argumentAst = astForNode(throwStmt.json("argument"))
    val throwCallNode =
      createCallNode(
        throwStmt.code,
        "<operator>.throw",
        DispatchTypes.STATIC_DISPATCH,
        throwStmt.lineNumber,
        throwStmt.columnNumber
      )
    callAst(throwCallNode, List(argumentAst))
  }

  private def astsForSwitchCase(switchCase: BabelNodeInfo): List[Ast] = {
    val labelAst = Ast(createJumpTarget(switchCase))
    val testAst = safeObj(switchCase.json, "test") match {
      case Some(test) => astForNode(Obj(test))
      case None       => Ast()
    }
    val consequentAsts = astForNodes(switchCase.json("consequent").arr.toList)
    List(labelAst, testAst) ++ consequentAsts
  }

  protected def astForSwitchStatement(switchStmt: BabelNodeInfo): Ast = {
    val switchNode = createControlStructureNode(switchStmt, ControlStructureTypes.SWITCH)

    val switchExpressionAst = astForNode(switchStmt.json("discriminant"))

    val blockId = createBlockNode(switchNode.code, switchStmt.lineNumber, switchNode.columnNumber)
    scope.pushNewBlockScope(blockId)
    localAstParentStack.push(blockId)

    val casesAsts = switchStmt.json("cases").arr.flatMap(c => astsForSwitchCase(createBabelNodeInfo(c)))
    setArgumentIndices(casesAsts.toList)

    scope.popScope()
    localAstParentStack.pop()

    setArgumentIndices(List(switchExpressionAst, Ast(blockId)))
    Ast(switchNode)
      .withChild(switchExpressionAst)
      .withConditionEdge(switchNode, switchExpressionAst.nodes.head)
      .withChild(Ast(blockId).withChildren(casesAsts))
  }

}
