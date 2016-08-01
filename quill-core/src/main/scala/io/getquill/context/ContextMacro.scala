package io.getquill.context

import scala.reflect.macros.whitebox.{ Context => MacroContext }

import io.getquill.ast.Ast
import io.getquill.ast.Dynamic
import io.getquill.dsl.CoreDsl
import io.getquill.quotation.FreeVariables
import io.getquill.quotation.Quotation
import io.getquill.util.Messages.RichContext

trait ContextMacro extends Quotation with ActionMacro with QueryMacro with QueryProbingMacro {
  val c: MacroContext
  import c.universe.{ Function => _, Ident => _, _ }

  protected def prepare(ast: Ast): Tree

  def run[R, S](quoted: Expr[_])(implicit r: WeakTypeTag[R], s: WeakTypeTag[S]): Tree = runExpr(quoted, true)(r, s)

  def runSingle[R, S](quoted: Expr[_])(implicit r: WeakTypeTag[R], s: WeakTypeTag[S]): Tree = runExpr(quoted, false)(r, s)

  private def runExpr[R, S](quoted: Expr[_], returnList: Boolean)(implicit r: WeakTypeTag[R], s: WeakTypeTag[S]): Tree = {
    implicit val t = c.WeakTypeTag(quoted.actualType.baseType(c.weakTypeOf[CoreDsl#Quoted[Any]].typeSymbol).typeArgs.head)

    val ast = this.ast(quoted)

    FreeVariables(ast) match {
      case free if free.isEmpty =>
      case free =>
        c.fail(s"""
          |Found the following free variables: ${free.mkString(", ")}.
          |Quotations can't reference values outside their scope directly. 
          |In order to bind runtime values to a quotation, please use the method `lift`.
          |Example: `def byName(n: String) = quote(query[Person].filter(_.name == lift(n)))`
        """.stripMargin)
    }

    run(quoted.tree, ast, returnList)(r, s, t)
  }

  private def run[R, S, T](quotedTree: Tree, ast: Ast, returnList: Boolean)(implicit r: WeakTypeTag[R], s: WeakTypeTag[S], t: WeakTypeTag[T]): Tree =
    ast match {
      case ast if ((t.tpe.erasure <:< c.weakTypeTag[CoreDsl#Action[Any]].tpe.erasure)) =>
        runAction[R, S, T](quotedTree, ast)

      case ast =>
        runQuery(quotedTree, ast, returnList)(r, s, queryType(t.tpe))
    }

  private def queryType(tpe: Type) =
    if (tpe <:< c.typeOf[CoreDsl#Query[_]])
      c.WeakTypeTag(tpe.baseType(c.typeOf[CoreDsl#Query[_]].typeSymbol).typeArgs.head)
    else
      c.WeakTypeTag(tpe)

  private def ast[T](quoted: Expr[_]) =
    unquote[Ast](quoted.tree).getOrElse {
      Dynamic(quoted.tree)
    }
}
