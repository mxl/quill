package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import io.getquill.dsl.CoreDsl
import java.io.Closeable
import scala.util.Try

abstract class Context[Statement] extends Closeable with CoreDsl {

  type RunQueryResult[T]
  type RunQuerySingleResult[T]
  type RunActionResult[T]
  type RunActionReturningResult[T]
  type RunActionBatchResult[T, O]

  val translator: Translator[Statement]

  def run[T](quoted: Quoted[T]): RunQuerySingleResult[T] = macro QueryMacro.runQuerySingle[Statement]
  def run[T](quoted: Quoted[Query[T]]): RunQueryResult[T] = macro QueryMacro.runQuery[Statement]
  def run[T](quoted: Quoted[Action[T]]): RunActionResult[T] = macro ActionMacro.runAction[Statement]
  def run[T, O](quoted: Quoted[ActionBatch[T, O]]): RunActionBatchResult[T, O] = macro ActionMacro.runActionBatch[T, O, Statement]
  def run[T](quoted: Quoted[ActionReturning[T]]): RunActionReturningResult[T] = macro ActionMacro.runActionReturning[Statement]

  def probe(statement: Statement): Try[_]

  protected def handleSingleResult[T](list: List[T]) =
    list match {
      case value :: Nil => value
      case other        => throw new IllegalStateException(s"Expected a single result but got $other")
    }
}
