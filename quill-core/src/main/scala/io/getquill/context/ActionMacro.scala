package io.getquill.context

import scala.reflect.macros.whitebox.{ Context => MacroContext }
import io.getquill.ast.Ident
import io.getquill.norm.select.SelectResultExtraction
import io.getquill.dsl.CoreDsl

class ActionMacro(val c: MacroContext) extends ContextMacro with EncodingMacro with SelectResultExtraction {
  import c.universe.{ Ident => _, _ }

  def runAction[Statement](quoted: Tree): Tree =
    expand[Statement](extractAst(quoted)) {
      (translated, bind) =>
        q"""
          ${c.prefix}.executeAction(
            $translated.statement,
            $bind
          )
        """
    }

  def runActionReturning[Statement](quoted: Tree): Tree = {
    val tpe = quotedType(quoted).baseType(c.typeOf[CoreDsl#ActionReturning[_]].typeSymbol).typeArgs.head
    expand[Statement](extractAst(quoted)) {
      (translated, bind) =>
        q"""
          ${c.prefix}.executeActionReturning(
            $translated.statement,
            $bind,
            ${returningExtractor(tpe)},
            $translated.returningColumn.get
          )
        """
    }
  }

  private def returningExtractor(returnType: c.Type) = {
    val selectValues = encoding(Ident("X"), returnType, Encoding.inferDecoder(c))
    selectResultExtractor(selectValues)
  }
}
