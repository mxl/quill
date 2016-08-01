package io.getquill

import scala.util.Failure
import scala.util.Success

import io.getquill.ast.Ast
import io.getquill.context.Context
import io.getquill.context.mirror.MirrorDecoders
import io.getquill.context.mirror.MirrorEncoders
import io.getquill.context.mirror.Row

import scala.language.experimental.macros
import io.getquill.context.mirror.MirrorContextMacro

private[getquill] object mirrorWithQueryProbing extends MirrorContextWithQueryProbing

class MirrorContextWithQueryProbing extends MirrorContext with QueryProbing

class MirrorContext
  extends Context
  with MirrorEncoders
  with MirrorDecoders {

  override type PrepareRow = Row
  override type ResultRow = Row

  override def close = ()

  def run[T](quoted: Quoted[Query[T]]): QueryMirror[T] = macro MirrorContextMacro.run[Row, Row]
  def run[T](quoted: Quoted[Action[T]]): ActionMirror = macro MirrorContextMacro.run[Row, Row]
  def run[T, O](quoted: Quoted[Returning[T, O]]): ActionMirror = macro MirrorContextMacro.run[Row, Row]
  def run[T](quoted: Quoted[T]): QueryMirror[T] = macro MirrorContextMacro.run[Row, Row]

  def probe(ast: Ast) =
    if (ast.toString.contains("Fail"))
      Failure(new IllegalStateException("The ast contains 'Fail'"))
    else
      Success(())

  case class ActionMirror(ast: Ast, bind: Row, returning: Option[String])

  def transaction[T](f: MirrorContext => T) = f(this)

  def executeAction[O](ast: Ast, bindParams: Row => Row = identity, returning: Option[String] = None, returningExtractor: Row => O = identity[Row] _) =
    ActionMirror(ast, bindParams(Row()), returning)

  case class BatchActionMirror(ast: Ast, bindList: List[Row], returning: Option[String])

  def executeActionBatch[T, O](ast: Ast, bindParams: T => Row => Row = (_: T) => identity[Row] _, returning: Option[String] = None, returningExtractor: Row => O = identity[Row] _) =
    (values: List[T]) =>
      BatchActionMirror(ast, values.map(bindParams).map(_(Row())), returning)

  case class QueryMirror[T](ast: Ast, binds: Row, extractor: Row => T)

  def executeQuerySingle[T](ast: Ast, extractor: Row => T = identity[Row] _, bind: Row => Row = identity) =
    QueryMirror(ast, bind(Row()), extractor)

  def executeQuery[T](ast: Ast, extractor: Row => T = identity[Row] _, bind: Row => Row = identity) = QueryMirror(ast, bind(Row()), extractor)
}
