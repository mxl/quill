package io.getquill.context

import scala.reflect.macros.whitebox.{ Context => MacroContext }

import io.getquill.util.InferImplicitValueWithFallback

trait Decoder[R, T] {
  def apply(index: Int, row: R): T
}

object Encoding {

  def inferDecoder[R](c: MacroContext)(tpe: c.Type)(implicit r: c.WeakTypeTag[R]) = {
    def decoderType[T](implicit t: c.WeakTypeTag[T]) = c.weakTypeTag[Decoder[R, T]]
    InferImplicitValueWithFallback(c)(decoderType(c.WeakTypeTag(tpe)).tpe, c.prefix.tree)
  }
}
