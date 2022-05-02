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
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodParameterIn
import io.shiftleft.codepropertygraph.generated.nodes.NewModifier
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn.PropertyDefaults
import ujson.Arr
import ujson.Value

import scala.collection.mutable

trait AstForFunctionsCreator {

  this: AstCreator =>

  private def handleParameters(
    parameters: Seq[Value],
    additionalBlockStatements: mutable.ArrayBuffer[Ast]
  ): Seq[Seq[NewMethodParameterIn]] = withIndex(parameters) { (param, index) =>
    createBabelNodeInfo(param) match {
      case rest @ BabelNodeInfo(BabelAst.RestElement) =>
        val paramName = rest.code.replace("...", "")
        val localId   = createLocalNode(paramName, Defines.ANY.label)
        diffGraph.addEdge(localAstParentStack.head, localId, EdgeTypes.AST)
        Seq(createParameterInNode(paramName, rest.code, index, isVariadic = true, rest.lineNumber, rest.columnNumber))
      case obj @ BabelNodeInfo(BabelAst.ObjectPattern) =>
        val objParams = withIndex(obj.json("properties").arr.toList) {
          case (objParam, i) if !objParam.isNull =>
            val objParamInfo = createBabelNodeInfo(objParam)
            val paramName    = code(objParamInfo.json("key"))
            val localId      = createLocalNode(paramName, Defines.ANY.label)
            diffGraph.addEdge(localAstParentStack.head, localId, EdgeTypes.AST)
            Some(
              createParameterInNode(
                paramName,
                objParamInfo.code,
                i,
                isVariadic = false,
                objParamInfo.lineNumber,
                objParamInfo.columnNumber
              )
            )
        }
        objParams.flatten
      case arr @ BabelNodeInfo(BabelAst.ArrayPattern) =>
        val arrParams = withIndex(arr.json("elements").arr.toList) {
          case (arrParam, i) if !arrParam.isNull =>
            val arrParamInfo = createBabelNodeInfo(arrParam)
            val paramName    = arrParamInfo.code
            val localId      = createLocalNode(paramName, Defines.ANY.label)
            diffGraph.addEdge(localAstParentStack.head, localId, EdgeTypes.AST)
            Some(
              createParameterInNode(
                paramName,
                arrParamInfo.code,
                i,
                isVariadic = false,
                arrParamInfo.lineNumber,
                arrParamInfo.columnNumber
              )
            )
          case _ => None
        }
        arrParams.flatten
      case assignmentPattern @ BabelNodeInfo(BabelAst.AssignmentPattern) =>
        val subTreeAst = createBabelNodeInfo(assignmentPattern.json("left")) match {
          case objPattern @ BabelNodeInfo(BabelAst.ObjectPattern) =>
            val rhsAst = astForNodeWithFunctionReference(assignmentPattern.json("right"))
            astForDeconstruction(objPattern, rhsAst)
          case arrPattern @ BabelNodeInfo(BabelAst.ArrayPattern) =>
            val rhsAst = astForNodeWithFunctionReference(assignmentPattern.json("right"))
            astForDeconstruction(arrPattern, rhsAst)
          case _ =>
            val key     = code(assignmentPattern.json("left"))
            val localId = createLocalNode(key, Defines.ANY.label)
            diffGraph.addEdge(localAstParentStack.head, localId, EdgeTypes.AST)
            convertDestructingParamWithDefault(assignmentPattern, key)
        }
        val blockChildren = List(subTreeAst)
        setIndices(blockChildren)
        additionalBlockStatements.addAll(blockChildren)
        val params = handleParameters(Seq(assignmentPattern.json("left")), additionalBlockStatements)
        params.flatten.map {
          case param if param.index == PropertyDefaults.Index => param.index(index)
          case param                                          => param
        }
      case other =>
        Seq(
          createParameterInNode(other.code, other.code, index, isVariadic = false, other.lineNumber, other.columnNumber)
        )
    }
  }

  private def convertDestructingParamWithDefault(element: BabelNodeInfo, key: String): Ast = {
    val lhsElement = element.json("left")
    val rhsElement = element.json("right")

    val rhsId = astForNodeWithFunctionReference(rhsElement)
    Ast.storeInDiffGraph(rhsId, diffGraph)

    val lhsId = createBabelNodeInfo(lhsElement) match {
      case objPattern @ BabelNodeInfo(BabelAst.ObjectPattern) => astForDeconstruction(objPattern, rhsId)
      case arrPattern @ BabelNodeInfo(BabelAst.ArrayPattern)  => astForDeconstruction(arrPattern, rhsId)
      case _                                                  => astForNode(lhsElement)
    }
    Ast.storeInDiffGraph(lhsId, diffGraph)

    val testId = {
      val keyId = createIdentifierNode(key, element)
      val voidCallId = createCallNode(
        "void 0",
        "<operator>.void",
        DispatchTypes.STATIC_DISPATCH,
        element.lineNumber,
        element.columnNumber
      )
      val equalsCallId =
        createEqualsCallAst(keyId, voidCallId, element.lineNumber, element.columnNumber)
      equalsCallId
    }
    Ast.storeInDiffGraph(testId, diffGraph)
    val falseId = createIdentifierNode(key, element)
    val ternaryNodeId =
      createTernaryCallAst(testId.nodes.head, rhsId.nodes.head, falseId, element.lineNumber, element.columnNumber)
    Ast.storeInDiffGraph(ternaryNodeId, diffGraph)
    val assignmentCallId =
      createAssignmentCallAst(
        lhsId.nodes.head,
        ternaryNodeId.nodes.head,
        s"${codeOf(lhsId.nodes.head)} = ${codeOf(ternaryNodeId.nodes.head)}",
        element.lineNumber,
        element.columnNumber
      )
    assignmentCallId
  }

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
    val blockAst                  = Ast(blockNode)
    val additionalBlockStatements = mutable.ArrayBuffer.empty[Ast]

    val capturingRefNode =
      if (shouldCreateFunctionReference) {
        methodRefNode
      } else {
        metaTypeRefIdStack.headOption
      }
    scope.pushNewMethodScope(methodFullName, methodName, blockNode, capturingRefNode)
    localAstParentStack.push(blockNode)

    val thisNode =
      createParameterInNode("this", "this", 0, isVariadic = false, line = func.lineNumber, column = func.columnNumber)

    val paramNodes = handleParameters(func.json("params").arr.toSeq, additionalBlockStatements)

    val bodyStmtAsts = func match {
      case BabelNodeInfo(BabelAst.ArrowFunctionExpression) => createBlockStatementAsts(Arr(block))
      case _                                               => createBlockStatementAsts(block("body"))
    }
    setIndices(additionalBlockStatements.toList ++ bodyStmtAsts)

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
      methodAst(
        methodNode,
        thisNode +: paramNodes.flatten,
        blockAst.withChildren(additionalBlockStatements ++ bodyStmtAsts),
        methodReturnNode
      )
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
