package io.joern.jssrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.datastructures.scope.BlockScope
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewModifier
import ujson.Arr

trait AstForFunctionsCreator {

  this: AstCreator =>

  protected def createMethodAstAndNode(
    func: BabelNodeInfo,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false
  ): (Ast, NewMethod) = {
    val (methodName, methodFullName) = calcMethodNameAndFullName(func)
    val methodRefNode = if (!shouldCreateFunctionReference) {
      None
    } else { Some(createMethodRefNode(methodName, methodFullName, func)) }

    val callAst = if (shouldCreateAssignmentCall && shouldCreateFunctionReference) {
      val idNode  = createIdentifierNode(methodName, func)
      val idLocal = createLocalNode(methodName, methodFullName)
      diffGraph.addEdge(localAstParentStack.head, idLocal, EdgeTypes.AST)
      scope.addVariable(methodName, idLocal, BlockScope)
      scope.addVariableReference(methodName, idNode)
      val code       = s"$methodName = ${func.code}"
      val assignment = createAssignmentCallAst(idNode, methodRefNode.get, code, func.lineNumber, func.columnNumber)
      assignment
    } else {
      Ast()
    }

    val methodNode          = createMethodNode(methodName, methodFullName, func)
    val virtualModifierNode = NewModifier().modifierType(ModifierTypes.VIRTUAL)

    methodAstParentStack.push(methodNode)

    val block             = func.json("body")
    val blockLineNumber   = line(block)
    val blockColumnNumber = column(block)
    val blockCode         = code(block)
    val blockNode = NewBlock()
      .typeFullName(Defines.ANY.label)
      .code(blockCode)
      .lineNumber(blockLineNumber)
      .columnNumber(blockColumnNumber)

    val capturingRefNode =
      if (shouldCreateFunctionReference) {
        methodRefNode
      } else {
        metaTypeRefIdStack.headOption
      }
    scope.pushNewMethodScope(methodFullName, methodName, blockNode, capturingRefNode)

    val thisNode =
      createParameterInNode("this", "this", 0, isVariadic = false, line = func.lineNumber, column = func.columnNumber)

    val paramNodes = withIndex(func.json("params").arr.toSeq) { (param, index) =>
      createBabelNodeInfo(param) match {
        case rest @ BabelNodeInfo(BabelAst.RestElement) =>
          Seq(
            Some(
              createParameterInNode(
                rest.code.replace("...", ""),
                rest.code,
                index,
                isVariadic = true,
                rest.lineNumber,
                rest.columnNumber
              )
            )
          )
        case arr @ BabelNodeInfo(BabelAst.ArrayPattern) =>
          val arrParams = withIndex(arr.json("elements").arr.toSeq) {
            case (arrParam, index) if !arrParam.isNull =>
              val arrParamInfo = createBabelNodeInfo(arrParam)
              Some(
                createParameterInNode(
                  arrParamInfo.code,
                  arrParamInfo.code,
                  index,
                  isVariadic = false,
                  arrParamInfo.lineNumber,
                  arrParamInfo.columnNumber
                )
              )
            case _ => None // skip null values
          }
          arrParams
        case other =>
          Seq(
            Some(
              createParameterInNode(
                other.code,
                other.code,
                index,
                isVariadic = false,
                other.lineNumber,
                other.columnNumber
              )
            )
          )
      }
    }.flatten

    localAstParentStack.push(blockNode)

    val bodyStmtAsts = func match {
      case BabelNodeInfo(BabelAst.ArrowFunctionExpression) => createBlockStatementAsts(Arr(block))
      case _                                               => createBlockStatementAsts(block("body"))
    }
    setIndices(bodyStmtAsts)

    val methodReturnNode = createMethodReturnNode(func)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(
        methodNode,
        methodAstParentStack.head,
        methodName,
        methodFullName,
        parserResult.filename
      )

    val mAst =
      methodAst(methodNode, thisNode +: paramNodes.flatten, Ast(blockNode).withChildren(bodyStmtAsts), methodReturnNode)
        .withChild(Ast(virtualModifierNode))

    Ast.storeInDiffGraph(mAst, diffGraph)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode, EdgeTypes.AST)

    val ast = methodRefNode match {
      case Some(ref) if callAst.nodes.isEmpty => Ast(ref)
      case _                                  => callAst
    }
    (ast, methodNode)
  }

  protected def astForFunctionDeclaration(
    func: BabelNodeInfo,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false
  ): Ast = createMethodAstAndNode(func, shouldCreateFunctionReference, shouldCreateAssignmentCall)._1

}
