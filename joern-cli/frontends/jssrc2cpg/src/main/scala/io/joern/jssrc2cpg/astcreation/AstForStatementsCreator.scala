package io.joern.jssrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.Operators
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
      case Some(test) => astForNodeWithFunctionReference(Obj(test))
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

  /** De-sugaring from:
    *
    * for (var i in/of arr) { body }
    *
    * to:
    *
    * { var _iterator = Object.keys(arr)[Symbol.iterator](); var _result; var i; while (!(_result =
    * _iterator.next()).done) { i = _result.value; body } }
    */
  protected def astForInOfStatement(forInOfStmt: BabelNodeInfo): Ast = {
    // surrounding block:
    val blockNode = createBlockNode(forInOfStmt.code, forInOfStmt.lineNumber, forInOfStmt.columnNumber)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)

    val collection     = forInOfStmt.json("right")
    val collectionName = code(collection)

    // _iterator assignment:
    val iteratorName      = generateUnusedVariableName(usedVariableNames, Set.empty, "_iterator")
    val iteratorLocalNode = createLocalNode(iteratorName, Defines.ANY.label)
    diffGraph.addEdge(localAstParentStack.head, iteratorLocalNode, EdgeTypes.AST)

    val iteratorNode = createIdentifierNode(iteratorName, forInOfStmt)

    val callNode = createCallNode(
      "Object.keys(" + collectionName + ")[Symbol.iterator]()",
      "",
      DispatchTypes.DYNAMIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val thisNode = createIdentifierNode("this", forInOfStmt)

    val indexCallNode = createCallNode(
      "Object.keys(" + collectionName + ")[Symbol.iterator]",
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val objectKeysCallNode = createStaticCallNode(
      "Object.keys(" + collectionName + ")",
      "keys",
      "Object.keys",
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val argAst            = astForNode(collection)
    val objectKeysCallAst = callAst(objectKeysCallNode, List(argAst))

    val indexBaseNode = createIdentifierNode("Symbol", forInOfStmt)

    val indexMemberNode = createFieldIdentifierNode("iterator", forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val indexAccessNode =
      createFieldAccess(indexBaseNode, indexMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val indexCallAst = callAst(indexCallNode, List(objectKeysCallAst, indexAccessNode))

    val callNodeAst = callAst(callNode, List(Ast(thisNode)), Some(indexCallAst))

    val iteratorAssignmentNode =
      createCallNode(
        iteratorName + " = " + "Object.keys(" + collectionName + ")[Symbol.iterator]()",
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH,
        forInOfStmt.lineNumber,
        forInOfStmt.columnNumber
      )

    val iteratorAssignmentAst = callAst(iteratorAssignmentNode, List(Ast(iteratorNode), callNodeAst))

    // _result:
    val resultName      = generateUnusedVariableName(usedVariableNames, Set.empty, "_result")
    val resultLocalNode = createLocalNode(resultName, Defines.ANY.label)
    diffGraph.addEdge(localAstParentStack.head, resultLocalNode, EdgeTypes.AST)
    val resultNode = createIdentifierNode(resultName, forInOfStmt)

    // loop variable:
    val loopVariableName = createBabelNodeInfo(forInOfStmt.json("left")) match {
      case v @ BabelNodeInfo(BabelAst.VariableDeclaration) => code(v.json("declarations").arr.head)
      case other                                           => code(other.json)
    }

    val loopVariableLocalNode = createLocalNode(loopVariableName, Defines.ANY.label)
    diffGraph.addEdge(localAstParentStack.head, loopVariableLocalNode, EdgeTypes.AST)
    val loopVariableNode = createIdentifierNode(loopVariableName, forInOfStmt)

    // while loop:
    val whileLoopNode =
      createControlStructureNode(forInOfStmt, ControlStructureTypes.WHILE)

    // while loop test:
    val testCallNode = createCallNode(
      "!(" + resultName + " = " + iteratorName + ".next()).done",
      Operators.not,
      DispatchTypes.STATIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val doneBaseNode = createCallNode(
      "(" + resultName + " = " + iteratorName + ".next())",
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val lhsNode = createIdentifierNode(resultName, forInOfStmt)

    val rhsNode = createCallNode(
      iteratorName + ".next()",
      "",
      DispatchTypes.DYNAMIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val nextBaseNode = createIdentifierNode(iteratorName, forInOfStmt)

    val nextMemberNode = createFieldIdentifierNode("next", forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val nextReceiverNode =
      createFieldAccess(nextBaseNode, nextMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val thisNextNode = createIdentifierNode(iteratorName, forInOfStmt)

    val rhsAst = callAst(rhsNode, List(Ast(thisNextNode)), Some(nextReceiverNode))

    val doneBaseAst = callAst(doneBaseNode, List(Ast(lhsNode), rhsAst))
    Ast.storeInDiffGraph(doneBaseAst, diffGraph)

    val doneMemberNode = createFieldIdentifierNode("done", forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val testNode = createFieldAccess(doneBaseNode, doneMemberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val testCallAst = callAst(testCallNode, List(testNode))

    val whileLoopAst = Ast(whileLoopNode).withChild(testCallAst).withConditionEdge(whileLoopNode, testCallNode)

    // while loop variable assignment:
    val whileLoopVariableNode = createIdentifierNode(loopVariableName, forInOfStmt)

    val baseNode = createIdentifierNode(resultName, forInOfStmt)

    val memberNode = createFieldIdentifierNode("value", forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val accessAst = createFieldAccess(baseNode, memberNode, forInOfStmt.lineNumber, forInOfStmt.columnNumber)

    val loopVariableAssignmentNode = createCallNode(
      loopVariableName + " = " + resultName + ".value",
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH,
      forInOfStmt.lineNumber,
      forInOfStmt.columnNumber
    )

    val loopVariableAssignmentAst = callAst(loopVariableAssignmentNode, List(Ast(whileLoopVariableNode), accessAst))

    val whileLoopBlockNode = createBlockNode(forInOfStmt.code, forInOfStmt.lineNumber, forInOfStmt.columnNumber)
    scope.pushNewBlockScope(whileLoopBlockNode)
    localAstParentStack.push(whileLoopBlockNode)

    // while loop block:
    val bodyAst = astForNode(forInOfStmt.json("body"))

    val whileLoopBlockAst = Ast(whileLoopBlockNode).withChild(loopVariableAssignmentAst).withChild(bodyAst)

    scope.popScope()
    localAstParentStack.pop()

    // end surrounding block:
    scope.popScope()
    localAstParentStack.pop()

    Ast(blockNode)
      .withChild(iteratorAssignmentAst)
      .withChild(Ast(resultNode))
      .withChild(Ast(loopVariableNode))
      .withChild(whileLoopAst.withChild(whileLoopBlockAst))
  }

}
