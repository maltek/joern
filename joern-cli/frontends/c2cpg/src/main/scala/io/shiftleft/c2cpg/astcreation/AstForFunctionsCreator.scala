package io.shiftleft.c2cpg.astcreation

import io.shiftleft.c2cpg.datastructures.Stack._
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.x2cpg.Ast
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLambdaExpression
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.c.{CASTFunctionDeclarator, CASTParameterDeclaration}
import org.eclipse.cdt.internal.core.dom.parser.cpp.{CPPASTFunctionDeclarator, CPPASTParameterDeclaration}

import scala.annotation.tailrec

trait AstForFunctionsCreator {

  this: AstCreator =>

  private def createFunctionTypeAndTypeDecl(method: NewMethod,
                                            methodName: String,
                                            methodFullName: String,
                                            signature: String): Ast = {
    val parentNode: NewTypeDecl = methodAstParentStack.collectFirst { case t: NewTypeDecl => t }.getOrElse {
      val astParentType = methodAstParentStack.head.label
      val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString
      val newTypeDeclNode = newTypeDecl(methodName, methodFullName, method.filename, astParentType, astParentFullName)
      Ast.storeInDiffGraph(Ast(newTypeDeclNode), diffGraph)
      newTypeDeclNode
    }

    method.astParentFullName = parentNode.fullName
    method.astParentType = parentNode.label
    val functionBinding = NewBinding().name(methodName).signature(signature)
    Ast(functionBinding).withBindsEdge(parentNode, functionBinding).withRefEdge(functionBinding, method)
  }

  @tailrec
  private def parameters(funct: IASTNode): Seq[IASTNode] = funct match {
    case decl: CPPASTFunctionDeclarator            => decl.getParameters.toIndexedSeq
    case decl: CASTFunctionDeclarator              => decl.getParameters.toIndexedSeq
    case defn: IASTFunctionDefinition              => parameters(defn.getDeclarator)
    case lambdaExpression: ICPPASTLambdaExpression => parameters(lambdaExpression.getDeclarator)
    case knr: ICASTKnRFunctionDeclarator           => knr.getParameterDeclarations.toIndexedSeq
    case other if other != null                    => notHandledYet(other, -1); Seq.empty
    case null                                      => Seq.empty
  }

  @tailrec
  private def isVariadic(funct: IASTNode): Boolean = funct match {
    case decl: CPPASTFunctionDeclarator            => decl.takesVarArgs()
    case decl: CASTFunctionDeclarator              => decl.takesVarArgs()
    case defn: IASTFunctionDefinition              => isVariadic(defn.getDeclarator)
    case lambdaExpression: ICPPASTLambdaExpression => isVariadic(lambdaExpression.getDeclarator)
    case _                                         => false
  }

  private def parameterListSignature(func: IASTNode, includeParamNames: Boolean): String = {
    val variadic = if (isVariadic(func)) "..." else ""
    val elements =
      if (!includeParamNames) parameters(func).map {
        case p: IASTParameterDeclaration => typeForDeclSpecifier(p.getDeclSpecifier)
        case other                       => typeForDeclSpecifier(other)
      } else {
        parameters(func).map(p => nodeSignature(p))
      }
    s"(${elements.mkString(",")}$variadic)"
  }

  protected def astForMethodRefForLambda(lambdaExpression: ICPPASTLambdaExpression): Ast = {
    val linenumber = line(lambdaExpression)
    val columnnumber = column(lambdaExpression)
    val filename = fileName(lambdaExpression)

    val returnType = lambdaExpression.getDeclarator match {
      case declarator: IASTDeclarator =>
        declarator.getTrailingReturnType match {
          case id: IASTTypeId => typeForDeclSpecifier(id.getDeclSpecifier)
          case _              => Defines.anyTypeName
        }
      case null => Defines.anyTypeName
    }
    val (name, fullname) = uniqueName("lambda", "", fullName(lambdaExpression))
    val signature = returnType + " " + fullname + " " + parameterListSignature(lambdaExpression,
                                                                               includeParamNames = false)
    val code = returnType + " " + name + " " + parameterListSignature(lambdaExpression, includeParamNames = true)
    val methodNode: NewMethod = NewMethod()
      .name(name)
      .code(code)
      .isExternal(false)
      .fullName(fullname)
      .lineNumber(linenumber)
      .lineNumberEnd(lineEnd(lambdaExpression))
      .columnNumber(columnnumber)
      .columnNumberEnd(columnEnd(lambdaExpression))
      .signature(signature)
      .filename(filename)

    scope.pushNewScope(methodNode)
    val parameterAsts = withOrder(parameters(lambdaExpression.getDeclarator)) { (p, o) =>
      astForParameter(p, o)
    }

    parameterAsts.lastOption.foreach(_.root.foreach {
      case p: NewMethodParameterIn if isVariadic(lambdaExpression) =>
        p.isVariadic = true
        p.code = p.code + "..."
      case _ =>
    })

    val lastOrder = 1 + parameterAsts.size

    val r = Ast(methodNode)
      .withChildren(parameterAsts)
      .withChild(astForMethodReturn(lambdaExpression, lastOrder, returnType))

    scope.popScope()
    val typeDeclAst = createFunctionTypeAndTypeDecl(methodNode, name, fullname, signature)

    Ast.storeInDiffGraph(r.merge(typeDeclAst), diffGraph)

    Ast(newMethodRefNode(code, fullname, methodNode.astParentFullName, lambdaExpression))
  }

  protected def astForFunctionDeclarator(funcDecl: IASTFunctionDeclarator, order: Int): Ast = {
    val linenumber = line(funcDecl)
    val columnnumber = column(funcDecl)
    val filename = fileName(funcDecl)

    val returnType = typeForDeclSpecifier(funcDecl.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclSpecifier)
    val name = shortName(funcDecl)
    val fullname = fullName(funcDecl)
    val templateParams = templateParameters(funcDecl).getOrElse("")
    val signature = returnType + " " + fullname + templateParams + " " + parameterListSignature(funcDecl,
                                                                                                includeParamNames =
                                                                                                  false)
    val code = returnType + " " + name + " " + parameterListSignature(funcDecl, includeParamNames = true)
    val methodNode = NewMethod()
      .name(name)
      .code(code)
      .isExternal(false)
      .fullName(fullname)
      .lineNumber(linenumber)
      .lineNumberEnd(lineEnd(funcDecl))
      .columnNumber(columnnumber)
      .columnNumberEnd(columnEnd(funcDecl))
      .signature(signature)
      .filename(filename)
      .order(order)

    scope.pushNewScope(methodNode)

    val parameterAsts = withOrder(parameters(funcDecl)) { (p, order) =>
      astForParameter(p, order)
    }

    parameterAsts.lastOption.foreach(_.root.foreach {
      case p: NewMethodParameterIn if isVariadic(funcDecl) =>
        p.isVariadic = true
        p.code = p.code + "..."
      case _ =>
    })

    val lastOrder = 1 + parameterAsts.size
    val r = Ast(methodNode)
      .withChildren(parameterAsts)
      .withChild(astForMethodReturn(funcDecl, lastOrder, returnType))

    scope.popScope()

    val typeDeclAst = createFunctionTypeAndTypeDecl(methodNode, name, fullname, signature)

    r.merge(typeDeclAst)
  }

  protected def astForFunctionDefinition(funcDef: IASTFunctionDefinition, order: Int): Ast = {
    val linenumber = line(funcDef)
    val columnnumber = column(funcDef)
    val filename = fileName(funcDef)

    val returnType = typeForDeclSpecifier(funcDef.getDeclSpecifier)
    val name = shortName(funcDef)
    val fullname = fullName(funcDef)
    val templateParams = templateParameters(funcDef).getOrElse("")
    val signature = returnType + " " + fullname + templateParams + " " + parameterListSignature(funcDef,
                                                                                                includeParamNames =
                                                                                                  false)
    val code = returnType + " " + name + " " + parameterListSignature(funcDef, includeParamNames = true)
    val methodNode = NewMethod()
      .name(name)
      .code(code)
      .isExternal(false)
      .fullName(fullname)
      .lineNumber(linenumber)
      .lineNumberEnd(lineEnd(funcDef))
      .columnNumber(columnnumber)
      .columnNumberEnd(columnEnd(funcDef))
      .signature(signature)
      .filename(filename)
      .order(order)

    methodAstParentStack.push(methodNode)
    scope.pushNewScope(methodNode)

    val parameterAsts = withOrder(parameters(funcDef)) { (p, order) =>
      astForParameter(p, order)
    }

    parameterAsts.lastOption.foreach(_.root.foreach {
      case p: NewMethodParameterIn if isVariadic(funcDef) =>
        p.isVariadic = true
        p.code = p.code + "..."
      case _ =>
    })

    val lastOrder = 1 + parameterAsts.size
    val r = Ast(methodNode)
      .withChildren(parameterAsts)
      .withChild(astForMethodBody(Option(funcDef.getBody), lastOrder))
      .withChild(astForMethodReturn(funcDef, lastOrder + 1, typeForDeclSpecifier(funcDef.getDeclSpecifier)))

    scope.popScope()
    methodAstParentStack.pop()

    val typeDeclAst = createFunctionTypeAndTypeDecl(methodNode, name, fullname, signature)

    r.merge(typeDeclAst)
  }

  private def astForParameter(parameter: IASTNode, childNum: Int): Ast = {
    val (name, code, tpe, variadic) = parameter match {
      case p: CASTParameterDeclaration =>
        (nodeSignature(p.getDeclarator.getName), nodeSignature(p), typeForDeclSpecifier(p.getDeclSpecifier), false)
      case p: CPPASTParameterDeclaration =>
        (nodeSignature(p.getDeclarator.getName),
         nodeSignature(p),
         typeForDeclSpecifier(p.getDeclSpecifier),
         p.getDeclarator.declaresParameterPack())
      case s: IASTSimpleDeclaration =>
        (s.getDeclarators.headOption
           .map(x => nodeSignature(x.getName))
           .getOrElse(uniqueName("parameter", "", "")._1),
         nodeSignature(s),
         typeForDeclSpecifier(s),
         false)
      case other =>
        (nodeSignature(other), nodeSignature(other), typeForDeclSpecifier(other), false)
    }

    val parameterNode = NewMethodParameterIn()
      .name(name)
      .code(code)
      .typeFullName(registerType(tpe))
      .order(childNum)
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .isVariadic(variadic)
      .lineNumber(line(parameter))
      .columnNumber(column(parameter))

    scope.addToScope(name, (parameterNode, tpe))

    Ast(parameterNode)
  }

  private def astForMethodBody(body: Option[IASTStatement], order: Int): Ast = {
    body match {
      case Some(b: IASTCompoundStatement) => astForBlockStatement(b, order)
      case None                           => Ast(NewBlock())
      case Some(b)                        => astForNode(b, order)
    }
  }

  private def astForMethodReturn(func: IASTNode, order: Int, tpe: String): Ast =
    Ast(
      NewMethodReturn()
        .order(order)
        .typeFullName(registerType(tpe))
        .code(tpe)
        .evaluationStrategy(EvaluationStrategies.BY_VALUE)
        .lineNumber(line(func))
        .columnNumber(column(func)))

}