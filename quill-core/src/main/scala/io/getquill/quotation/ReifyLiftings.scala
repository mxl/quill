package io.getquill.quotation

import io.getquill.util.Messages._
import io.getquill.ast._
import scala.reflect.macros.whitebox.{ Context => MacroContext }
import scala.reflect.NameTransformer
import io.getquill.dsl.EncodingDsl
import io.getquill.dsl.macroz.LiftingMacro
import io.getquill.norm.BetaReduction

case class ScalarLifting[T](value: T, encoder: EncodingDsl#Encoder[T])
case class CaseClassLifting[T](value: T)

trait ReifyLiftings extends LiftingMacro {
  val c: MacroContext
  import c.universe._

  private val liftings = TermName("liftings")

  private def encode(name: String) =
    TermName(NameTransformer.encode(name))

  private case class Reified(value: Tree, encoder: Option[Tree])

  private case class ReifyLiftings(state: collection.Map[TermName, Reified])
    extends StatefulTransformer[collection.Map[TermName, Reified]] {

    private def reify(lift: Lift) =
      lift match {
        case ScalarLift(name, value: Tree, encoder: Tree)      => Reified(value, Some(encoder))
        case CaseClassLift(name, value: Tree)                  => Reified(value, None)
        case ScalarBatchLift(name, value: Tree, encoder: Tree) => Reified(value, Some(encoder))
        case CaseClassBatchLift(name, value: Tree)             => Reified(value, None)
      }

    override def apply(ast: Ast) =
      ast match {

        case ast: Lift =>
          (ast, ReifyLiftings(state + (encode(ast.name) -> reify(ast))))

        case Property(CaseClassLift(name, v: Tree), prop) =>

          val merge = c.typecheck(q"$v.${TermName(prop)}")

          lift(merge)(c.WeakTypeTag(merge.tpe)) match {

            case q"$ctx.lift($value, $encoder)" =>
              apply(ScalarLift(merge.toString, value, encoder))

            case q"$ctx.liftBatch($value, $encoder)" =>
              apply(ScalarBatchLift(merge.toString, value, encoder))

            case other =>
              c.fail("Can't reify nested case classes.")
          }

        case QuotedReference(ref: Tree, refAst) =>
          val newAst =
            Transform(refAst) {
              case lift: Lift =>
                val nested =
                  q"""
                    {
                      import scala.language.reflectiveCalls
                      $ref.$liftings.${encode(lift.name)}
                    }  
                  """
                lift match {
                  case ScalarLift(name, value, encoder) =>
                    ScalarLift(s"$ref.$name", q"$nested.value", q"$nested.encoder")
                  case CaseClassLift(name, value) =>
                    CaseClassLift(s"$ref.$name", q"$nested.value")
                  case ScalarBatchLift(name, value, encoder) =>
                    ScalarBatchLift(s"$ref.$name", q"$nested.value", q"$nested.encoder")
                  case CaseClassBatchLift(name, value) =>
                    CaseClassBatchLift(s"$ref.$name", q"$nested.value")
                }
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
                encoder match {
                  case Some(encoder) =>
                    q"val $name = io.getquill.quotation.ScalarLifting($value, $encoder)"
                  case None =>
                    q"val $name = io.getquill.quotation.CaseClassLifting($value)"
                }
              }
            (ast, q"val $liftings = new { ..$trees }")
        }
    }
}
