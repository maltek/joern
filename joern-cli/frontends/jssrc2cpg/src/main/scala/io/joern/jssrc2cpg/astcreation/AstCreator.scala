package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelJsonParser.ParseResult
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.datastructures.scope.BlockScopeElement
import io.joern.jssrc2cpg.datastructures.scope.MethodScope
import io.joern.jssrc2cpg.datastructures.scope.MethodScopeElement
import io.joern.jssrc2cpg.datastructures.scope.ResolvedReference
import io.joern.jssrc2cpg.datastructures.scope.Scope
import io.joern.jssrc2cpg.datastructures.scope.ScopeType
import io.joern.jssrc2cpg.parser.BabelAst.BabelNode
import io.joern.jssrc2cpg.passes.Defines
import io.joern.jssrc2cpg.passes.GlobalBuiltins
import AstCreatorHelper.OptionSafeAst
import io.joern.jssrc2cpg.datastructures.scope.BlockScope
import io.joern.jssrc2cpg.datastructures.scope.ScopeElement
import io.joern.jssrc2cpg.datastructures.scope.ScopeElementIterator
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.AstCreatorBase
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.Operators
import org.slf4j.{Logger, LoggerFactory}
import ujson.Value

import scala.collection.mutable
import scala.util.Try

class AstCreator(val config: Config, val parserResult: ParseResult, val global: Global)
    extends AstCreatorBase(parserResult.filename)
    with AstNodeBuilder
    with AstCreatorHelper {

  protected case class BabelNodeInfo(
    node: BabelNode,
    json: Value,
    code: String,
    lineNumber: Option[Integer],
    columnNumber: Option[Integer],
    lineNumberEnd: Option[Integer],
    columnNumberEnd: Option[Integer]
  )

  object BabelNodeInfo {
    def unapply(other: BabelNodeInfo): Some[BabelNode] = Some(other.node)
  }

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope = new Scope()

  // TypeDecls with their bindings (with their refs) for lambdas and methods are not put in the AST
  // where the respective nodes are defined. Instead we put them under the parent TYPE_DECL in which they are defined.
  // To achieve this we need this extra stack.
  protected val methodAstParentStack: Stack[NewNode]          = new Stack[NewNode]()
  protected val localAstParentStack: Stack[NewBlock]          = new Stack[NewBlock]()
  protected val dynamicInstanceTypeStack: mutable.Seq[String] = mutable.Stack.empty[String]

  protected val usedVariableNames: mutable.HashMap[String, Int] = mutable.HashMap.empty[String, Int]
  private val functionNodeToNameAndFullName                     = mutable.HashMap.empty[BabelNodeInfo, (String, String)]
  private val functionFullNames                                 = mutable.HashSet.empty[String]
  private val metaTypeRefIdStack                                = mutable.Stack.empty[NewTypeRef]

  override def createAst(): DiffGraphBuilder = {
    val name    = parserResult.filename
    val cpgFile = Ast(NewFile().name(name).order(0))
    val ast     = cpgFile.withChild(astForFileGlobal())
    Ast.storeInDiffGraph(ast, diffGraph)
    createVariableReferenceLinks()
    diffGraph
  }

  private def createBabelNodeInfo(json: Value): BabelNodeInfo = {
    val c     = shortenCode(code(json))
    val ln    = line(json)
    val cn    = column(json)
    val lnEnd = lineEnd(json)
    val cnEnd = columnEnd(json)
    val node  = nodeType(json)
    BabelNodeInfo(node, json, c, ln, cn, lnEnd, cnEnd)
  }

  protected def astsForExpressionStatement(exprStmt: BabelNodeInfo, order: Int): Seq[Ast] =
    astsForNode(exprStmt.json("expression"), order)

  private def createBuiltinStaticCall(
    callExpr: BabelNodeInfo,
    callee: BabelNodeInfo,
    methodFullName: String,
    order: Int
  ): Ast = {
    val methodName = callee.node match {
      case BabelAst.MemberExpression =>
        code(callee.json("property"))
      case BabelAst.Identifier =>
        callee.code
      case _ => callee.code
    }
    val callId =
      createStaticCallNode(callExpr.code, methodName, methodFullName, order, callee.lineNumber, callee.columnNumber)
    val args = astsForNodes(callExpr.json("arguments").arr.toSeq)
    Ast(callId).withChildren(args).withArgEdges(callId, args)
  }

  private def handleCallNodeArgs(
    callExpr: BabelNodeInfo,
    receiverId: NewNode,
    baseId: NewNode,
    functionBaseId: NewNode,
    functionPropertyId: Option[NewFieldIdentifier]
  ): Seq[Ast] = {
    val args = astsForNodes(callExpr.json("arguments").arr.toSeq)

    val baseCode = codeOf(functionBaseId)
    val propertyCode = functionPropertyId match {
      case Some(id) => "." + codeOf(id)
      case None     => ""
    }

    val argsCode = args.map(a => codeOf(a.nodes.head)).mkString("(", ", ", ")")
    val code     = s"$baseCode$propertyCode$argsCode"

    val callId = createCallNode(code, "", DispatchTypes.DYNAMIC_DISPATCH, callExpr.lineNumber, callExpr.columnNumber)

    addOrder(receiverId, 0)
    addOrder(baseId, 1)
    addArgumentIndex(baseId, 0)

    var currOrder    = 2
    var currArgIndex = 1
    args.foreach { ast =>
      addOrder(ast.nodes.head, currOrder)
      addArgumentIndex(ast.nodes.head, currArgIndex)
      currOrder += 1
      currArgIndex += 1
    }

    Seq(
      Ast(callId)
        .withChild(Ast(receiverId))
        .withReceiverEdge(callId, receiverId)
        .withChild(Ast(baseId))
        .withArgEdge(callId, baseId)
        .withChildren(args)
        .withArgEdges(callId, args)
    )
  }

  private def astsForCallExpression(callExpr: BabelNodeInfo, order: Int): Seq[Ast] = {
    val callee         = createBabelNodeInfo(callExpr.json("callee"))
    val methodFullName = callee.code
    val callId = if (GlobalBuiltins.builtins.contains(methodFullName)) {
      Seq(createBuiltinStaticCall(callExpr, callee, methodFullName, order))
    } else {
      val (functionBaseId, functionPropertyId, receiverId, baseId) = callee.node match {
        case BabelAst.MemberExpression => // functionAccessNode
          // "this" argument is coming from source.
          val base = createBabelNodeInfo(callee.json("object"))
          base.node match {
            case BabelAst.Identifier =>
              val receiverId = astsForNode(callee.json, 0)
              val baseId     = createIdentifierNode(base.code, base).order(1).argumentIndex(1)
              scope.addVariableReference(base.code, baseId)
              (receiverId, None, receiverId, baseId)
            case _ =>
              // TODO: check for used nodes
              val tmpVarName = generateUnusedVariableName(usedVariableNames, Set.empty, "_tmp")
              val baseTmpId  = createIdentifierNode(tmpVarName, base)
              scope.addVariableReference(tmpVarName, baseTmpId)
              val baseId = astsForNode(base.json, 2)
              val code   = s"(${codeOf(baseTmpId)} = ${base.code})"
              val tmpAssignmentId =
                createAssignment(baseTmpId, baseId.head.nodes.head, code, 1, base.lineNumber, base.columnNumber)
              val memberNode = createBabelNodeInfo(callee.json("property"))
              val memberId = createFieldIdentifierNode(memberNode.code, memberNode.lineNumber, memberNode.columnNumber)
              val fieldAccessId =
                createFieldAccessNode(tmpAssignmentId.nodes.head, memberId, 0, callee.lineNumber, callee.columnNumber)
              val thisTmpId = createIdentifierNode(tmpVarName, callee)
              scope.addVariableReference(tmpVarName, thisTmpId)

              Ast.storeInDiffGraph(tmpAssignmentId, diffGraph)
              Ast.storeInDiffGraph(fieldAccessId, diffGraph)
              Ast.storeInDiffGraph(Ast(memberId), diffGraph)

              (baseId, Some(memberId), Seq(fieldAccessId), thisTmpId)
          }
        case _ =>
          val receiverId = astsForNode(callee.json, order)
          val thisId     = createIdentifierNode("this", callee)
          scope.addVariableReference("this", thisId)
          (receiverId, None, receiverId, thisId)
      }
      handleCallNodeArgs(
        callExpr,
        receiverId.head.nodes.head,
        baseId,
        functionBaseId.head.nodes.head,
        functionPropertyId
      )
    }
    callId
  }

  private def astsForMemberExpression(memberExpr: BabelNodeInfo, order: Int): Seq[Ast] = {
    val baseId = astsForNode(memberExpr.json("object"), 1)
    val memberId =
      createFieldIdentifierNode(code(memberExpr.json("property")), memberExpr.lineNumber, memberExpr.columnNumber)
    val accessId =
      createFieldAccessNode(baseId.head.nodes.head, memberId, order, memberExpr.lineNumber, memberExpr.columnNumber)
    Seq(accessId)
  }

  protected def astsForIdentifier(ident: BabelNodeInfo, order: Int): Seq[Ast] = {
    val name    = ident.json("name").str
    val identId = createIdentifierNode(name, ident).order(order).argumentIndex(order)
    scope.addVariableReference(name, identId)
    Seq(Ast(identId))
  }

  protected def astsForStringLiteral(stringLiteral: BabelNodeInfo, order: Int): Seq[Ast] =
    Seq(
      Ast(
        createLiteralNode(
          stringLiteral.code,
          Some(Defines.STRING.label),
          order,
          stringLiteral.lineNumber,
          stringLiteral.columnNumber
        )
      )
    )

  protected def astsForNumericLiteral(numericLiteral: BabelNodeInfo, order: Int): Seq[Ast] =
    Seq(
      Ast(
        createLiteralNode(
          numericLiteral.code,
          Some(Defines.NUMBER.label),
          order,
          numericLiteral.lineNumber,
          numericLiteral.columnNumber
        )
      )
    )

  private def computeScopePath(stack: Option[ScopeElement]): String =
    new ScopeElementIterator(stack)
      .to(Seq)
      .reverse
      .collect { case methodScopeElement: MethodScopeElement =>
        methodScopeElement.name
      }
      .mkString(":")

  private def calcMethodNameAndFullName(func: BabelNodeInfo): (String, String) = {
    def calcMethodName(func: BabelNodeInfo): String = {
      val name = func match {
        // case _ if func.isAnonymous && func.isClassConstructor =>
        //  "anonClass<constructor>"
        // case _ if func.isAnonymous => TODO: is this a ArrowFunctionExpression?
        //  "anonymous"
        // case _ if func.isClassConstructor =>
        //  s"${func.getName}<constructor>"
        case _ if func.json("id").isNull =>
          "anonymous"
        case _ =>
          func.json("id")("name").str
      }
      name
    }

    // functionNode.getName is not necessarily unique and thus the full name calculated based on the scope
    // is not necessarily unique. Specifically we have this problem with lambda functions which are defined
    // in the same scope.
    functionNodeToNameAndFullName.get(func) match {
      case Some(nameAndFullName) =>
        nameAndFullName
      case None =>
        val intendedName   = calcMethodName(func)
        val fullNamePrefix = parserResult.filename + ":" + computeScopePath(scope.getScopeHead) + ":"
        var name           = intendedName
        var fullName       = ""

        var isUnique = false
        var i        = 1
        while (!isUnique) {
          fullName = fullNamePrefix + name
          if (functionFullNames.contains(fullName)) {
            name = intendedName + i.toString
            i += 1
          } else {
            isUnique = true
          }
        }

        functionFullNames.add(fullName)
        functionNodeToNameAndFullName(func) = (name, fullName)
        (name, fullName)
    }
  }

  protected def astsForFunctionDeclaration(
    func: BabelNodeInfo,
    order: Int,
    shouldCreateFunctionReference: Boolean = false
  ): Seq[Ast] = {
    val bodyStmts = func.json("body")("body").arr.toSeq
    val params    = func.json("params").arr.toSeq

    val (methodName, methodFullName) = calcMethodNameAndFullName(func)

    val methodId          = createMethodNode(methodName, methodFullName, func).order(order)
    val virtualModifierId = NewModifier().modifierType(ModifierTypes.VIRTUAL)
    val methodRefId = if (!shouldCreateFunctionReference) {
      None
    } else { Some(createMethodRefNode(methodName, methodFullName, func)) }

    methodAstParentStack.push(methodId)

    val block             = func.json("body")
    val blockLineNumber   = line(block)
    val blockColumnNumber = column(block)
    val blockCode         = code(block)
    val blockId = NewBlock()
      .typeFullName(Defines.ANY.label)
      .code(blockCode)
      .lineNumber(blockLineNumber)
      .columnNumber(blockColumnNumber)

    val capturingRefId =
      if (shouldCreateFunctionReference) {
        methodRefId
      } else {
        metaTypeRefIdStack.headOption
      }
    scope.pushNewMethodScope(methodFullName, methodName, blockId, capturingRefId)

    val thisId = createParameterInNode("this", "this", 0, line = func.lineNumber, column = func.columnNumber)

    val paramIds = withOrder(params) { (p, o) =>
      createBabelNodeInfo(p) match {
        case rest @ BabelNodeInfo(BabelAst.RestElement) =>
          Ast(
            createParameterInNode(rest.code.replace("...", ""), rest.code, o, rest.lineNumber, rest.columnNumber)
              .isVariadic(true)
          )
        case other =>
          Ast(createParameterInNode(other.code, other.code, o, other.lineNumber, other.columnNumber))
      }
    }

    localAstParentStack.push(blockId)

    val bodyStmtsIds = astsForNodes(bodyStmts)

    val methodReturnId = createMethodReturnNode(func).order(2)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()
    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(
        methodId,
        methodAstParentStack.head,
        methodName,
        methodFullName,
        parserResult.filename
      )

    val methodAst = Ast(methodId)
      .withChild(Ast(virtualModifierId))
      .withChild(Ast(thisId))
      .withChildren(paramIds)
      .withChild(Ast(blockId).withChildren(bodyStmtsIds))
      .withChild(Ast(methodReturnId))

    Ast.storeInDiffGraph(methodAst, diffGraph)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodId, EdgeTypes.AST)

    methodRefId.map(Ast(_)).toSeq
  }

  protected def astsForVariableDeclarator(declarator: Value, order: Int, scopeType: ScopeType): Seq[Ast] = {
    val id   = createBabelNodeInfo(declarator("id"))
    val init = Try(createBabelNodeInfo(declarator("init"))).toOption

    val typeFullName = init match {
      case Some(f @ BabelNodeInfo(BabelAst.FunctionDeclaration)) =>
        val (_, methodFullName) = calcMethodNameAndFullName(f)
        methodFullName
      case _ => Defines.ANY.label
    }

    val varId = createLocalNode(id.code, typeFullName, 0)
    scope.addVariable(id.code, varId, scopeType)

    init match {
      case Some(initExpr) =>
        val destId   = astsForNode(id.json, 1)
        val sourceId = astsForNode(initExpr.json, 2)
        val assigmentCallId =
          createAssignment(
            destId.head.nodes.head,
            sourceId.head.nodes.head,
            code(declarator),
            order,
            line = initExpr.lineNumber,
            column = initExpr.columnNumber
          )
        Seq(Ast(varId), assigmentCallId) ++ destId ++ sourceId
      case None => Seq(Ast(varId))
    }
  }

  protected def astsForVariableDeclaration(declaration: BabelNodeInfo, order: Int): Seq[Ast] = {
    val scopeType = if (declaration.json("kind").str == "let") {
      BlockScope
    } else {
      MethodScope
    }

    withOrder(declaration.json("declarations").arr.toSeq) { (d, o) =>
      astsForVariableDeclarator(d, order + o - 1, scopeType)
    }.flatten
  }

  protected def astsForAssignmentExpression(assignment: BabelNodeInfo, order: Int): Seq[Ast] = {
    val op = assignment.json("operator").str match {
      case "="    => Operators.assignment
      case "+="   => Operators.assignmentPlus
      case "-="   => Operators.assignmentMinus
      case "*="   => Operators.assignmentMultiplication
      case "/="   => Operators.assignmentDivision
      case "%="   => Operators.assignmentModulo
      case "**="  => Operators.assignmentExponentiation
      case "&="   => Operators.assignmentAnd
      case "&&="  => Operators.assignmentAnd
      case "|="   => Operators.assignmentOr
      case "||="  => Operators.assignmentOr
      case "^="   => Operators.assignmentXor
      case "<<="  => Operators.assignmentShiftLeft
      case ">>="  => Operators.assignmentArithmeticShiftRight
      case ">>>=" => Operators.assignmentLogicalShiftRight
      case "??="  => Operators.notNullAssert
      case other =>
        logger.warn(s"Unknown assignment operator: '$other'")
        Operators.assignment
    }

    val lhsId = astsForNode(assignment.json("left"), 1)
    val rhsId = astsForNode(assignment.json("right"), 2)

    val callId =
      createCallNode(assignment.code, op, DispatchTypes.STATIC_DISPATCH, assignment.lineNumber, assignment.columnNumber)
        .order(order)

    Seq(Ast(callId).withChildren(lhsId).withChildren(rhsId).withArgEdges(callId, lhsId).withArgEdges(callId, rhsId))
  }

  protected def astsForBlockStatement(block: BabelNodeInfo, order: Int): Seq[Ast] = {
    val blockId = createBlockNode(block.code, order, block.lineNumber, block.columnNumber)
    scope.pushNewBlockScope(blockId)
    localAstParentStack.push(blockId)

    val blockStatements = withOrder(block.json("body").arr.toSeq) { (s, o) =>
      astsForNode(s, o)
    }.flatten

    localAstParentStack.pop()
    scope.popScope()

    Seq(Ast(blockId).withChildren(blockStatements))
  }

  protected def astsForNode(json: Value, order: Int): Seq[Ast] = createBabelNodeInfo(json) match {
    case BabelNodeInfo(BabelAst.File)                              => astsForNode(json("program"), order)
    case BabelNodeInfo(BabelAst.Program)                           => astsForNodes(json("body").arr.toSeq)
    case exprStmt @ BabelNodeInfo(BabelAst.ExpressionStatement)    => astsForExpressionStatement(exprStmt, order)
    case callExpr @ BabelNodeInfo(BabelAst.CallExpression)         => astsForCallExpression(callExpr, order)
    case memberExpr @ BabelNodeInfo(BabelAst.MemberExpression)     => astsForMemberExpression(memberExpr, order)
    case ident @ BabelNodeInfo(BabelAst.Identifier)                => astsForIdentifier(ident, order)
    case stringLiteral @ BabelNodeInfo(BabelAst.StringLiteral)     => astsForStringLiteral(stringLiteral, order)
    case numLiteral @ BabelNodeInfo(BabelAst.NumericLiteral)       => astsForNumericLiteral(numLiteral, order)
    case func @ BabelNodeInfo(BabelAst.FunctionDeclaration)        => astsForFunctionDeclaration(func, order)
    case decl @ BabelNodeInfo(BabelAst.VariableDeclaration)        => astsForVariableDeclaration(decl, order)
    case assignment @ BabelNodeInfo(BabelAst.AssignmentExpression) => astsForAssignmentExpression(assignment, order)
    case block @ BabelNodeInfo(BabelAst.BlockStatement)            => astsForBlockStatement(block, order)
    case other                                                     => Seq(notHandledYet(other, order))
  }

  protected def astsForNodes(jsons: Seq[Value]): Seq[Ast] = withOrder(jsons) { (n, o) =>
    astsForNode(n, o)
  }.flatten

  private def createProgramMethod(path: String): Ast = {
    val allDecls        = Seq(parserResult.json("ast"))
    val lineNumber      = allDecls.headOption.flatMap(line)
    val columnNumber    = allDecls.headOption.flatMap(column)
    val lineNumberEnd   = allDecls.headOption.flatMap(lineEnd)
    val columnNumberEnd = allDecls.headOption.flatMap(columnEnd)
    val name            = ":program"
    val fullName        = parserResult.filename + ":" + name

    val programMethod =
      NewMethod()
        .name(name)
        .code(name)
        .fullName(fullName)
        .filename(path)
        .lineNumber(lineNumber)
        .lineNumberEnd(lineNumberEnd)
        .columnNumber(columnNumber)
        .columnNumberEnd(columnNumberEnd)
        .astParentType(NodeTypes.TYPE_DECL)
        .astParentFullName(fullName)

    methodAstParentStack.push(programMethod)

    val blockNode = NewBlock()
      .order(1)
      .argumentIndex(1)
      .typeFullName("ANY")

    scope.pushNewMethodScope(fullName, name, blockNode, None)
    localAstParentStack.push(blockNode)

    val thisParam = createParameterInNode("this", "this", 0, lineNumber, columnNumber)

    var currOrder = 1
    val methodChildren = allDecls.flatMap { node =>
      val r = astsForNode(node, currOrder)
      currOrder = currOrder + r.length
      r
    }.toIndexedSeq

    val methodReturn = NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName("ANY")
      .order(2)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(programMethod, methodAstParentStack.head, name, fullName, path)

    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)

    Ast(programMethod)
      .withChild(Ast(thisParam))
      .withChild(Ast(blockNode).withChildren(methodChildren))
      .withChild(Ast(methodReturn))
  }

  private def astForFileGlobal(): Ast = {
    val absolutePath = parserResult.filename
    val name         = NamespaceTraversal.globalNamespaceName
    val fullName     = MetaDataPass.getGlobalNamespaceBlockFullName(Some(absolutePath))
    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .fullName(fullName)
      .filename(absolutePath)
      .order(1)
    methodAstParentStack.push(namespaceBlock)
    Ast(namespaceBlock).withChild(createProgramMethod(absolutePath))
  }

  private def createVariableReferenceLinks(): Unit = {
    val resolvedReferenceIt = scope.resolve(createMethodLocalForUnresolvedReference)
    val capturedLocals      = mutable.HashMap.empty[String, NewNode]

    resolvedReferenceIt.foreach { case ResolvedReference(variableNodeId, origin) =>
      var currentScope             = origin.stack
      var currentReferenceId       = origin.referenceNodeId
      var nextReferenceId: NewNode = null

      var done = false
      while (!done) {
        val localOrCapturedLocalIdOption =
          if (currentScope.get.nameToVariableNode.contains(origin.variableName)) {
            done = true
            Some(variableNodeId)
          } else {
            currentScope.flatMap {
              case methodScope: MethodScopeElement =>
                // We have reached a MethodScope and still did not find a local variable to link to.
                // For all non local references the CPG format does not allow us to link
                // directly. Instead we need to create a fake local variable in method
                // scope and link to this local which itself carries the information
                // that it is a captured variable. This needs to be done for each
                // method scope until we reach the originating scope.
                val closureBindingIdProperty =
                  methodScope.methodFullName + ":" + origin.variableName
                capturedLocals
                  .updateWith(closureBindingIdProperty) {
                    case None =>
                      val methodScopeNodeId = methodScope.scopeNode
                      val localId =
                        createLocalNode(origin.variableName, Defines.ANY.label, 0, Some(closureBindingIdProperty))
                      diffGraph.addEdge(methodScopeNodeId, localId, EdgeTypes.AST)
                      val closureBindingId = createClosureBindingNode(closureBindingIdProperty, origin.variableName)
                      methodScope.capturingRefId.foreach(ref =>
                        diffGraph.addEdge(ref, closureBindingId, EdgeTypes.CAPTURE)
                      )
                      nextReferenceId = closureBindingId
                      Some(localId)
                    case someLocalId =>
                      // When there is already a LOCAL representing the capturing, we do not
                      // need to process the surrounding scope element as this has already
                      // been processed.
                      done = true
                      someLocalId
                  }
              case _: BlockScopeElement => None
            }
          }

        localOrCapturedLocalIdOption.foreach { localOrCapturedLocalId =>
          diffGraph.addEdge(currentReferenceId, localOrCapturedLocalId, EdgeTypes.REF)
          currentReferenceId = nextReferenceId
        }

        currentScope = currentScope.get.surroundingScope
      }
    }
  }

  private def createMethodLocalForUnresolvedReference(
    methodScopeNodeId: NewNode,
    variableName: String
  ): (NewNode, ScopeType) = {
    val varId = createLocalNode(variableName, Defines.ANY.label, 0)
    diffGraph.addEdge(methodScopeNodeId, varId, EdgeTypes.AST)
    (varId, MethodScope)
  }

}
