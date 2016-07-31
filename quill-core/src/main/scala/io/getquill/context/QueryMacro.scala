package io.getquill.context

import scala.reflect.macros.whitebox.{ Context => MacroContext }

import io.getquill.ast.Ast
import io.getquill.ast.Ident
import io.getquill.ast.Map
import io.getquill.ast.Query
import io.getquill.norm.Normalize
import io.getquill.norm.select.SelectFlattening
import io.getquill.norm.select.SelectResultExtraction

trait QueryMacro extends SelectFlattening with SelectResultExtraction {
  this: ContextMacro =>

  val c: MacroContext
  import c.universe.{ Ident => _, _ }

  def runQuery[R, S, T](
    quotedTree: Tree,
    ast:        Ast,
    returnList: Boolean
  )(
    implicit
    r: WeakTypeTag[R], s: WeakTypeTag[S], t: WeakTypeTag[T]
  ): Tree = {

    val query = Normalize(ast) match {
      case q: Query => q
      case q        => Map(q, Ident("x"), Ident("x"))
    }
    val queryMethod = if (returnList) TermName("executeQuery") else TermName("executeQuerySingle")
    val (flattenQuery, selectValues) = flattenSelect[T](query, Encoding.inferDecoder[R](c))
    val extractor = selectResultExtractor[R](selectValues)
    q"""
    {
      val quoted = $quotedTree
      val (sql, liftings: List[io.getquill.ast.Lift], _) =
          ${prepare(flattenQuery)}
          
      val bind =
        (row: $r) => 
          (liftings.foldLeft((0, row)) {
            case ((idx, row), lift) =>
              (idx + 1, lift.encoder(idx, lift.value, row))
          })._2

      ${c.prefix}.$queryMethod(
        sql,
        $extractor,
        bind)
    }
    """
  }
}
