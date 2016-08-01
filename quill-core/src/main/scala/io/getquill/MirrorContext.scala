package io.getquill

import scala.util.Failure
import scala.util.Success

import io.getquill.ast.Ast
import io.getquill.context.Context
import io.getquill.context.mirror.MirrorDecoders
import io.getquill.context.mirror.MirrorEncoders
import io.getquill.context.mirror.Row

import io.getquill.context.mirror.MirrorTranslator

private[getquill] object mirrorWithQueryProbing extends MirrorContextWithQueryProbing

class MirrorContextWithQueryProbing extends MirrorContext with QueryProbing

class MirrorContext
  extends Context[Ast]
  with MirrorEncoders
  with MirrorDecoders {

  override type PrepareRow = Row
  override type ResultRow = Row

  override type RunQueryResult[T] = QueryMirror[T]
  override type RunQuerySingleResult[T] = QueryMirror[T]
  override type RunActionResult[T] = ActionMirror
  override type RunActionReturningResult[T] = ActionReturningMirror[T]

  override def close = ()

  def probe(statement: Ast) =
    if (statement.toString.contains("Fail"))
      Failure(new IllegalStateException("The ast contains 'Fail'"))
    else
      Success(())

  override val translator = MirrorTranslator

  def transaction[T](f: => T) = f

  case class ActionMirror(ast: Ast, prepareRow: PrepareRow)
  case class ActionReturningMirror[O](ast: Ast, prepareRow: Row, extractor: Row => O, returningColumn: String)
  case class QueryMirror[T](ast: Ast, prepareRow: Row, extractor: Row => T)

  def executeAction(ast: Ast, prepare: Row => Row = identity) =
    ActionMirror(ast, prepare(Row()))

  def executeActionReturning[O](ast: Ast, prepare: Row => Row = identity, extractor: Row => O, returningColumn: String) =
    ActionReturningMirror[O](ast, prepare(Row()), extractor, returningColumn)

  def executeQuery[T](ast: Ast, prepare: Row => Row = identity, extractor: Row => T = identity[Row] _) =
    QueryMirror(ast, prepare(Row()), extractor)

  def executeQuerySingle[T](ast: Ast, prepare: Row => Row = identity, extractor: Row => T = identity[Row] _) =
    QueryMirror(ast, prepare(Row()), extractor)
}
