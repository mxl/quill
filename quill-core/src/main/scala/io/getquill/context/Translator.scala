package io.getquill.context

import io.getquill.ast.Ast
import io.getquill.ast.CollectAst
import io.getquill.ast.Lift
import io.getquill.ast.Returning

trait Translator[Statement] {

  def statementLiftable(c: ContextMacro): c.c.universe.Liftable[Statement]

  def translate(ast: Ast): Translated[Statement] =
    Translated(
      statement(ast),
      returningColumn(ast),
      liftings(ast)
    )

  def statement(ast: Ast): Statement

  def returningColumn(ast: Ast) =
    CollectAst.byType[Returning](ast)
      .headOption.map(_.property)

  def liftings(ast: Ast) = CollectAst.byType[Lift](ast)
}

trait AstLiftable {
  this: Translator[Ast] =>
  override def statementLiftable(ctx: ContextMacro) = {
    import ctx._
    implicitly[c.universe.Liftable[Ast]]
  }
}

trait StringLiftable {
  this: Translator[String] =>
  override def statementLiftable(ctx: ContextMacro) = {
    import ctx.c.universe._
    implicitly[Liftable[String]]
  }
}

case class Translated[Statement](
  statement:       Statement,
  returningColumn: Option[String],
  liftings:        List[Lift]
)
