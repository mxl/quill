package io.getquill.context

import scala.reflect.macros.whitebox.{ Context => MacroContext }

import io.getquill.ast.AssignedAction
import io.getquill.ast.Assignment
import io.getquill.ast.Ast
import io.getquill.ast.Ident
import io.getquill.ast.Property
import io.getquill.dsl.CoreDsl
import io.getquill.util.Messages.fail

trait ActionMacro extends EncodingMacro {
  this: ContextMacro =>

  val c: MacroContext
  import c.universe.{ Ident => _, _ }

  def runAction[R, S, T](
    quotedTree: Tree,
    action: Ast)(
      implicit r: WeakTypeTag[R],
      s: WeakTypeTag[S],
      t: WeakTypeTag[T]): Tree =
    expandedTreeSingle[R](quotedTree, action, returningType(t.tpe))

  private def expandedTreeSingle[R](quotedTree: Tree, action: Ast, returningType: Type)(implicit r: WeakTypeTag[R]) =
    q"""
    {
      val quoted = $quotedTree
      val (sql, bind: ($r => $r), returning) =
        ${prepare(action)}

      ${c.prefix}.executeAction[$returningType](
        sql,
        bind,
        returning,
        ${returningExtractor(returningType)(r)}
        )
    }
    """

  private def returningExtractor[R](returnType: c.Type)(implicit r: WeakTypeTag[R]) = {
    val returnWeakTypeTag = c.WeakTypeTag(returnType)
    val selectValues = encoding(Ident("X"), Encoding.inferDecoder[R](c))(returnWeakTypeTag)
    selectResultExtractor[R](selectValues)
  }

  private def returningType(tpe: Type): Type = tpe.baseType(c.typeOf[CoreDsl#Action[_, _]].typeSymbol).typeArgs(1)

}
