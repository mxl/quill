package io.getquill.quotation

import io.getquill.util.Messages._
import io.getquill.ast._
import scala.reflect.macros.whitebox.{ Context => MacroContext }
import scala.reflect.NameTransformer
import io.getquill.dsl.EncodingDsl
import io.getquill.dsl.macroz.LiftingMacro
import io.getquill.norm.BetaReduction

case class Lifting[T](value: T, encoder: EncodingDsl#Encoder[T])

trait ReifyLiftings extends LiftingMacro {
  val c: MacroContext
  import c.universe._

  private val liftings = TermName("liftings")

  private def encode(name: String) =
    TermName(NameTransformer.encode(name))

  private case class Reified(value: Tree, encoder: Tree)

  private case class ReifyLiftings(state: collection.Map[TermName, Reified])
    extends StatefulTransformer[collection.Map[TermName, Reified]] {

    override def apply(ast: Ast) =
      ast match {

        case ast @ Lift(name, value: Tree, encoder: Tree) =>
          (ast, ReifyLiftings(state + (encode(name) -> Reified(value, encoder))))

        case Property(CaseClassLift(v: Tree), prop) =>

          val merge = c.typecheck(q"$v.${TermName(prop)}")

          lift(merge)(c.WeakTypeTag(merge.tpe)) match {

            case q"$ctx.lift($value, $encoder)" =>
              apply(Lift(merge.toString, value, encoder))

            case q"$ctx.liftCaseClass($value)" =>
              c.fail("Can't reify nested case classes.")
          }

        case QuotedReference(ref: Tree, refAst) =>
          val newAst =
            Transform(refAst) {
              case Lift(name, value: Tree, encoder: Tree) =>
                val nested =
                  q"""
                    {
                      import scala.language.reflectiveCalls
                      $ref.$liftings.${encode(name)}
                    }  
                  """
                Lift(s"$ref.$name", q"$nested.value", q"$nested.encoder")
            }
          apply(newAst)

        case other => super.apply(other)
      }
  }

  protected def reifyLiftings(ast: Ast): (Ast, Tree) =
    ReifyLiftings(collection.Map.empty)(ast) match {
      case (ast, _) =>
        // reify again with beta reduction, given that the first pass will remove `QuotedReference`s 
        ReifyLiftings(collection.Map.empty)(BetaReduction(ast)) match {
          case (ast, transformer) =>
            val trees =
              for ((name, Reified(value, encoder)) <- transformer.state) yield {
                q"val $name = io.getquill.quotation.Lifting($value, $encoder)"
              }
            (ast, q"val $liftings = new { ..$trees }")
        }
    }
}
