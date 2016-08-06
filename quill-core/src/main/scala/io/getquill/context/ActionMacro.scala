package io.getquill.context

import scala.reflect.macros.whitebox.{ Context => MacroContext }
import io.getquill.ast.Ident
import io.getquill.norm.select.SelectResultExtraction
import io.getquill.dsl.CoreDsl
import io.getquill.ast._
import io.getquill.quotation.ReifyLiftings

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

  def runActionBatch[T, O, Statement](quoted: Tree)(implicit t: WeakTypeTag[T], o: WeakTypeTag[O]): Tree = {
    //    val (list, (ast, _)) =
    //      BetaReduction(extractAst(quoted)) match {
    //        case BatchAction(list: Tree, Function(List(alias), action)) =>
    //          val lift = Lift("value", q"value", inferRequiredEncoder(t.tpe))
    //          (list, reifyLiftings(BetaReduction(action, alias -> lift)))
    //        case other =>
    //          c.fail(s"Batch actions must be static quotations. Found: '$other'")
    //      }
    //    expand[Statement](ast) {
    //      (translated, bind) =>
    //        q"""
    //          ${c.prefix}.executeActionBatch[$t, $o](
    //            $translated.statement,
    //            $list.map(value => $bind)
    //          )
    //        """
    //    }
    ???
  }

  private def returningExtractor(returnType: c.Type) = {
    val selectValues = encoding(Ident("X"), returnType, Encoding.inferDecoder(c))
    selectResultExtractor(selectValues)
  }
}
