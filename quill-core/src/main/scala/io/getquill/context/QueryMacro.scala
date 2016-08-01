package io.getquill.context

import scala.reflect.macros.whitebox.{ Context => MacroContext }

import io.getquill.ast.Ident
import io.getquill.ast.Map
import io.getquill.ast.Query
import io.getquill.norm.Normalize
import io.getquill.norm.select.SelectFlattening
import io.getquill.norm.select.SelectResultExtraction
import io.getquill.dsl.CoreDsl

class QueryMacro(val c: MacroContext) extends ContextMacro with SelectFlattening with SelectResultExtraction {
  import c.universe.{ Ident => _, _ }

  def runQuery[Statement](quoted: Tree): Tree = {
    val tpe = quotedType(quoted).baseType(c.typeOf[CoreDsl#Query[_]].typeSymbol).typeArgs.head
    expandQuery(quoted, "executeQuery", tpe)
  }

  def runQuerySingle[Statement](quoted: Tree): Tree =
    expandQuery(quoted, "executeQuerySingle", quotedType(quoted))

  private def expandQuery[Statement](quoted: Tree, method: String, tpe: Type) = {
    val ast = extractAst(quoted)
    val query = Normalize(ast) match {
      case q: Query => q
      case q        => Map(q, Ident("x"), Ident("x"))
    }
    val (flattenAst, selectValues) = flattenSelect(query, tpe, Encoding.inferDecoder(c))
    val extractor = selectResultExtractor(selectValues)

    expand[Statement](flattenAst) {
      (translated, bind) =>
        q"""
          ${c.prefix}.${TermName(method)}(
            $translated.statement,
            $bind,
            $extractor
          )  
        """
    }
  }
}
