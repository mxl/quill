package io.getquill.ast

import scala.reflect.api.Trees

//************************************************************

sealed trait Ast {
  override def toString = {
    import io.getquill.ast.AstShow._
    import io.getquill.util.Show._
    this.show
  }
}

//************************************************************

sealed trait Query extends Ast

sealed trait Entity extends Query {
  def properties: List[PropertyAlias]
  def alias: Option[String]
}

object Entity {
  def apply(name: String) = SimpleEntity(name)
}

case class SimpleEntity(name: String) extends Entity {
  def properties: List[PropertyAlias] = List()
  def alias: Option[String] = None
}

case class ConfiguredEntity(
  source:     Ast,
  alias:      Option[String]      = None,
  properties: List[PropertyAlias] = List()
) extends Entity

case class PropertyAlias(property: String, alias: String)

case class Filter(query: Ast, alias: Ident, body: Ast) extends Query

case class Map(query: Ast, alias: Ident, body: Ast) extends Query

case class FlatMap(query: Ast, alias: Ident, body: Ast) extends Query

case class SortBy(query: Ast, alias: Ident, criterias: Ast, ordering: Ast) extends Query

sealed trait Ordering extends Ast
case class TupleOrdering(elems: List[Ordering]) extends Ordering

sealed trait PropertyOrdering extends Ordering
case object Asc extends PropertyOrdering
case object Desc extends PropertyOrdering
case object AscNullsFirst extends PropertyOrdering
case object DescNullsFirst extends PropertyOrdering
case object AscNullsLast extends PropertyOrdering
case object DescNullsLast extends PropertyOrdering

case class GroupBy(query: Ast, alias: Ident, body: Ast) extends Query

case class Aggregation(operator: AggregationOperator, ast: Ast) extends Query

case class Take(query: Ast, n: Ast) extends Query

case class Drop(query: Ast, n: Ast) extends Query

case class Union(a: Ast, b: Ast) extends Query

case class UnionAll(a: Ast, b: Ast) extends Query

case class Join(typ: JoinType, a: Ast, b: Ast, aliasA: Ident, aliasB: Ident, on: Ast) extends Query

case class Distinct(a: Ast) extends Query

//************************************************************

case class Infix(parts: List[String], params: List[Ast]) extends Ast

case class Function(params: List[Ident], body: Ast) extends Ast

case class Ident(name: String) extends Ast

case class Property(ast: Ast, name: String) extends Ast

case class OptionOperation(t: OptionOperationType, ast: Ast, alias: Ident, body: Ast) extends Ast

case class If(condition: Ast, `then`: Ast, `else`: Ast) extends Ast

//************************************************************

sealed trait Operation extends Ast

case class UnaryOperation(operator: UnaryOperator, ast: Ast) extends Operation
case class BinaryOperation(a: Ast, operator: BinaryOperator, b: Ast) extends Operation
case class FunctionApply(function: Ast, values: List[Ast]) extends Operation

//************************************************************

sealed trait Value extends Ast

case class Constant(v: Any) extends Value

object NullValue extends Value

case class Tuple(values: List[Ast]) extends Value

case class Collection(values: List[Ast]) extends Value

//************************************************************

case class Block(statements: List[Ast]) extends Ast

case class Val(name: Ident, body: Ast) extends Ast

//************************************************************

sealed trait Action extends Ast

case class Update(query: Ast, assignments: List[Assignment]) extends Action
case class Insert(query: Ast, assignments: List[Assignment]) extends Action
case class Delete(query: Ast) extends Action

case class Returning(action: Ast, property: String) extends Action

//case class ActionAll(batch: Ast, alias: Ast, assignments

case class Assignment(input: Ident, property: String, value: Ast)

//************************************************************

case class Dynamic(tree: Any) extends Ast

case class QuotedReference(tree: Trees#Tree, ast: Ast) extends Ast

case class Lift(name: String, value: Any, encoder: Any) extends Ast

case class CaseClassLift(value: Any) extends Ast
