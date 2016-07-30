package io.getquill.context

import scala.reflect.ClassTag

import io.getquill.dsl.CoreDsl
import java.io.Closeable
import io.getquill.WrappedType

abstract class Context[R: ClassTag, S: ClassTag] extends Closeable with CoreDsl {

  protected def handleSingleResult[T](list: List[T]) =
    list match {
      case value :: Nil => value
      case other        => throw new IllegalStateException(s"Expected a single result but got $other")
    }
}
