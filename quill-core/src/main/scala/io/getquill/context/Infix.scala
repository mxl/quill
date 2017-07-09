package io.getquill.context

import java.util.Date

trait Infix {
  this: Context[_, _] =>

  trait ComparisonOperators[T] {
    val left: T

    def >(right: T) = quote(infix"$left > $right".as[Boolean])

    def <(right: T) = quote(infix"$left < $right".as[Boolean])

    def >=(right: T) = quote(infix"$left >= $right".as[Boolean])

    def <=(right: T) = quote(infix"$left <= $right".as[Boolean])
  }

  implicit class DateOperators(val left: Date) extends ComparisonOperators[Date]

}
