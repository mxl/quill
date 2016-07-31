package io.getquill.context

import io.getquill.dsl.CoreDsl
import java.io.Closeable

abstract class Context extends Closeable with CoreDsl {

  protected def handleSingleResult[T](list: List[T]) =
    list match {
      case value :: Nil => value
      case other        => throw new IllegalStateException(s"Expected a single result but got $other")
    }
}
