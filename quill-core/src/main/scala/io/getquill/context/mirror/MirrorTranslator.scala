package io.getquill.context.mirror

import io.getquill.ast.Ast
import io.getquill.norm.Normalize
import io.getquill.context.Translator
import io.getquill.context.AstLiftable

object MirrorTranslator extends Translator[Ast] with AstLiftable {
  override def statement(ast: Ast) = Normalize(ast)
}