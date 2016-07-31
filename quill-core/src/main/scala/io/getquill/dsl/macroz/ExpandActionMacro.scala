package io.getquill.dsl.macroz

import io.getquill.util.Messages._
import scala.reflect.macros.whitebox.{ Context => MacroContext }

trait ExpandActionMacro {
  val c: MacroContext
  import c.universe._

  def expandInsert[T](value: Expr[T])(implicit t: WeakTypeTag[T]): Tree =
    expandAction(value, "insert")

  def expandUpdate[T](value: Expr[T])(implicit t: WeakTypeTag[T]): Tree =
    expandAction(value, "update")

  private def expandAction[T](value: Expr[T], method: String)(implicit t: WeakTypeTag[T]): Tree = {
    caseClassConstructor(t.tpe) match {
      case None => c.fail("Can't expand a non-case class")
      case Some(constructor) =>
        val assignments =
          constructor.paramLists.flatten.map { param =>
            val term = param.name.toTermName
            q"(v: $t) => v.$term -> $value.$term"
          }
        q"${c.prefix}.${TermName(method)}(..$assignments)"
    }
  }

  private def caseClassConstructor(t: Type) =
    t.members.collect {
      case m: MethodSymbol if (m.isPrimaryConstructor) => m
    }.headOption
}