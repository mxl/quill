package io.getquill.context

import scala.reflect.macros.whitebox.{ Context => MacroContext }

import io.getquill.ast.Ast
import io.getquill.ast.Dynamic
import io.getquill.quotation.Quotation
import io.getquill.util.LoadObject
import io.getquill.util.Messages._
import io.getquill.dsl.CoreDsl
import io.getquill.quotation.IsDynamic

trait ContextMacro extends Quotation {
  val c: MacroContext
  import c.universe.{ Function => _, Ident => _, _ }

  protected def expand[Statement](ast: Ast)(call: (Tree, Tree) => Tree) = {
    val translated = translate(ast)
    val bind =
      q"""
        (row: ${c.prefix}.PrepareRow) =>
          ($translated.liftings.foldLeft((0, row)) {
            case ((idx, row), lift) =>
              val newRow =
                lift.encoder.asInstanceOf[${c.prefix}.Encoder[Any]](idx, lift.value, row)
              (idx + 1, newRow)
          })._2
      """
    call(translated, bind)
  }

  private def translate[Statement](ast: Ast): Tree =
    IsDynamic(ast) match {
      case false => translateStatic(ast)
      case true  => translateDynamic(ast)
    }

  private def translateStatic[Statement](ast: Ast): Tree = {
    val translator = LoadObject[Translator[Statement]](c)(c.typecheck(q"${c.prefix}.translator").tpe)
    val translated = translator.translate(ast)
    import translated._

    ProbeStatement(statement, c)
    c.info(statement.toString)

    implicit val statementLiftable = translator.statementLiftable(this)

    q"io.getquill.context.Translated($statement, $returningColumn, $liftings)"
  }

  private def translateDynamic[Statement](ast: Ast): Tree =
    q"${c.prefix}.translator.translate($ast)"

  def quotedType(tree: Tree) =
    tree.tpe.baseType(c.typeOf[CoreDsl#Quoted[_]].typeSymbol).typeArgs.head

  def extractAst[T](quoted: Tree) =
    unquote[Ast](quoted)
      .map(VerifyFreeVariables(c))
      .getOrElse {
        Dynamic(quoted)
      }
}
