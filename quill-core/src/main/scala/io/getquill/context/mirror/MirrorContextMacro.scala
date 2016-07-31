package io.getquill.context.mirror

import scala.reflect.macros.whitebox.{ Context => MacroContext }

//import io.getquill.MirrorContext
import io.getquill.ast._
import io.getquill.context.ContextMacro
import io.getquill.norm.Normalize
import io.getquill.quotation.IsDynamic
import io.getquill.util.Messages.RichContext
import io.getquill.MirrorContext

class MirrorContextMacro(val c: MacroContext) extends ContextMacro {
  import c.universe.{ Ident => _, _ }

  override protected def prepare(ast: Ast) =
    IsDynamic(ast) match {
      case false =>
        val returning = CollectAst(ast) {
          case Returning(_, property) => property
        }.headOption

        val liftings = CollectAst(ast) {
          case Lift(_, value: Tree, encoder: Tree) => (value, encoder)
        }

        val normalized = Normalize(ast)

        probeQuery[MirrorContext](_.probe(normalized))
        c.info(normalized.toString)

        q"($normalized, List(..$liftings).asInstanceOf[List[(Any, ${c.prefix}.Encoder[Any])]], $returning)"
      case true =>
        q"""
          import io.getquill.norm._
          import io.getquill.ast._

          val ast = ${ast: Ast}: Ast

          val returning = CollectAst(ast) {
            case Returning(_, property) => property
          }.headOption
          
          val liftings = CollectAst(ast) {
            case l: Lift => l
          }

          val normalized = Normalize(ast)

          (normalized, liftings, returning)
        """
    }
}
