package io.getquill.context

import io.getquill.util.Messages._
import scala.reflect.macros.whitebox.{ Context => MacroContext }
import io.getquill.ast.Ident
import io.getquill.norm.select.SelectResultExtraction
import io.getquill.dsl.CoreDsl
import io.getquill.ast._
import io.getquill.quotation.ReifyLiftings
import io.getquill.norm.BetaReduction
import io.getquill.quotation.IsDynamic

class ActionMacro(val c: MacroContext)
    extends ContextMacro
    with EncodingMacro
    with SelectResultExtraction
    with ReifyLiftings {
  import c.universe.{ Ident => _, Function => _, _ }

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

  def runBatchAction[T, Statement](quoted: Tree)(implicit t: WeakTypeTag[T]): Tree = {
    val (ast, batch) =
      extractAst(quoted) match {
        case ast if (IsDynamic(ast)) => c.fail(s"Batch actions must be static quotations")
        case ast =>
          BetaReduction(ast) match {
            case Foreach(ScalarBatchLift(name, batch: Tree, encoder: Tree), alias, body) =>
              val (ast, _) = reifyLiftings(BetaReduction(body, alias -> ScalarLift("value", q"value", encoder)))
              (ast, batch)
            case other =>
              c.fail(s"Batch actions must be static quotations. Found: '$other'")
          }
      }
    val batchItemType = batch.tpe.typeArgs.head
    expand[Statement](ast) {
      (translated, bind) =>
        q"""
          ${c.prefix}.executeActionBatch[$batchItemType, $t](
            $translated.statement,
            $batch,
            value => $bind
          )
        """
    }
  }

  private def returningExtractor(returnType: c.Type) = {
    val selectValues = encoding(Ident("X"), returnType, Encoding.inferDecoder(c))
    selectResultExtractor(selectValues)
  }
}
