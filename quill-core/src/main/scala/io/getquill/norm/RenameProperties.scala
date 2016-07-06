package io.getquill.norm

import io.getquill.ast._
import io.getquill.ast.Entity
import io.getquill.ast.Filter
import io.getquill.ast.FlatMap
import io.getquill.ast.Ident
import io.getquill.ast.Join
import io.getquill.ast.Map
import io.getquill.ast.Property
import io.getquill.ast.SortBy
import io.getquill.ast.StatefulTransformer

case class RenameProperties(state: Tuple)
  extends StatefulTransformer[Tuple] {

  override def apply(ast: Ast): (Ast, StatefulTransformer[Tuple]) =
    ast match {
      case ast: Entity =>
        (ast, RenameProperties(Tuple(List(ast))))

      case Join(t, a, b, iA, iB, o) =>
        val (ar, art) = apply(a)
        val (br, brt) = apply(b)
        val ora = apply(art.state, iA, o)
        val orb = apply(brt.state, iB, ora)
        val rt = RenameProperties(Tuple(List(art.state, brt.state)))
        (Join(t, ar, br, iA, iB, orb), rt)

      case FlatMap(q, x, p) =>
        apply(q, x, p)(FlatMap) match {
          case (FlatMap(q, x, p), _) =>
            val (pr, prt) = apply(p)
            (FlatMap(q, x, pr), prt)
        }

      case Map(q, x, p)       => apply(q, x, p)(Map)
      case Filter(q, x, p)    => apply(q, x, p)(Filter)
      case SortBy(q, x, p, o) => apply(q, x, p)(SortBy(_, _, _, o))
      case GroupBy(q, x, p)   => apply(q, x, p)(GroupBy)

      case other              => super.apply(other)
    }

  def apply[T](q: Ast, x: Ident, p: Ast)(f: (Ast, Ident, Ast) => T): (T, StatefulTransformer[Tuple]) = {
    val (qr, qrt) = apply(q)
    val pr = apply(qrt.state, x, p)
    (f(qr, x, pr), qrt)
  }

  def apply[T](t: Tuple, x: Ident, p: Ast) = {
    def reductionMap(base: Ast, tuple: Ast): collection.Map[Ast, Ast] =
      tuple match {
        case Tuple(List(e: Entity)) =>
          (for (PropertyAlias(prop, alias) <- e.properties) yield {
            Property(base, prop) -> Property(base, alias)
          }).toMap
        case Tuple(Nil) =>
          collection.Map.empty
        case Tuple(tuples) =>
          (for ((tuple, idx) <- tuples.zipWithIndex) yield {
            reductionMap(Property(base, s"_${idx + 1}"), tuple)
          }).reduce(_ ++ _)
      }
    BetaReduction(reductionMap(x, t))(p)
  }
}

object RenameProperties {
  def apply(q: Ast) =
    new RenameProperties(Tuple(List()))(q) match {
      case (q, _) => q
    }
}
