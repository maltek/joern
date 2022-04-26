package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast

trait AstForPrimitivesCreator {

  this: AstCreator =>

  protected def astForIdentifier(ident: BabelNodeInfo): Ast = {
    val name      = ident.json("name").str
    val identNode = createIdentifierNode(name, ident)
    scope.addVariableReference(name, identNode)
    Ast(identNode)
  }

  protected def astForNullLiteral(nullLiteral: BabelNodeInfo): Ast =
    Ast(createLiteralNode(nullLiteral.code, Some(Defines.NULL.label), nullLiteral.lineNumber, nullLiteral.columnNumber))

  protected def astForStringLiteral(stringLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        stringLiteral.code,
        Some(Defines.STRING.label),
        stringLiteral.lineNumber,
        stringLiteral.columnNumber
      )
    )

  protected def astForRegExpLiteral(regExpLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        regExpLiteral.code,
        Some(Defines.STRING.label),
        regExpLiteral.lineNumber,
        regExpLiteral.columnNumber
      )
    )

  protected def astForRegexLiteral(regexLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        regexLiteral.code,
        Some(Defines.STRING.label),
        regexLiteral.lineNumber,
        regexLiteral.columnNumber
      )
    )

  protected def astForNumberLiteral(numberLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        numberLiteral.code,
        Some(Defines.NUMBER.label),
        numberLiteral.lineNumber,
        numberLiteral.columnNumber
      )
    )

  protected def astForNumericLiteral(numericLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        numericLiteral.code,
        Some(Defines.NUMBER.label),
        numericLiteral.lineNumber,
        numericLiteral.columnNumber
      )
    )

  protected def astForDecimalLiteral(decimalLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        decimalLiteral.code,
        Some(Defines.NUMBER.label),
        decimalLiteral.lineNumber,
        decimalLiteral.columnNumber
      )
    )

  protected def astForBigIntLiteral(bigIntLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        bigIntLiteral.code,
        Some(Defines.NUMBER.label),
        bigIntLiteral.lineNumber,
        bigIntLiteral.columnNumber
      )
    )

  protected def astForBooleanLiteral(booleanLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        booleanLiteral.code,
        Some(Defines.BOOLEAN.label),
        booleanLiteral.lineNumber,
        booleanLiteral.columnNumber
      )
    )

}
