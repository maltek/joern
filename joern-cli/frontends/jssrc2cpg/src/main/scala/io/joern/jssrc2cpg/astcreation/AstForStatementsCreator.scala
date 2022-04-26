package io.joern.jssrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
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

}
