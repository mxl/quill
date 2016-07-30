package io.getquill.quotation

import io.getquill.ast._
import scala.reflect.macros.whitebox.{ Context => MacroContext }
import scala.reflect.NameTransformer
import io.getquill.dsl.EncodingDsl

case class Lifting[T](value: T, encoder: EncodingDsl#Encoder[T])

trait ReifyLiftings {
  val c: MacroContext
  import c.universe._

  private val liftings = TermName("liftings")

  private def name(tree: Tree) =
    TermName(NameTransformer.encode(tree.toString))
    
  private case class Reified(value: Tree, encoder: Tree)

  private case class ReifyLiftings(state: collection.Map[TermName, Reified])
    extends StatefulTransformer[collection.Map[TermName, Reified]] {

    override def apply(ast: Ast) =
      ast match {

        case ast @ Lift(value: Tree, encoder: Tree) =>
          (ast, ReifyLiftings(state + (name(value) -> Reified(value, encoder))))

        case QuotedReference(ref: Tree, refAst) =>
          val newAst =
            Transform(refAst) {
              case Lift(value: Tree, encoder: Tree) =>
                val nested = q"$ref.$liftings.${name(value)}"
                Lift(q"$nested.value", q"$nested.encoder")
            }
          apply(newAst)
      }
  }

  protected def reifyLiftings(ast: Ast): (Ast, Tree) =
    ReifyLiftings(collection.Map.empty)(ast) match {
      case (ast, ReifyLiftings(map)) =>
        val trees =
          for ((name, Reified(value, encoder)) <- map) yield {
            q"val $name = ${c.symbolOf[Lifting[_]]}($value, $encoder)"
          }
        (ast, q"val $liftings = new { ..$trees }")
    }
}
