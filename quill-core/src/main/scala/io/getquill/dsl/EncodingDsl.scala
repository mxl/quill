package io.getquill.dsl

import io.getquill.WrappedType
import scala.annotation.compileTimeOnly
import io.getquill.quotation.NonQuotedException
import language.experimental.macros
import io.getquill.lifting.LiftingMacro

trait EncodingDsl {

  type PrepareRow
  type ResultRow

  type Decoder[T] = io.getquill.context.Decoder[ResultRow, T]

  trait Encoder[-T] {
    def apply(index: Int, value: T, row: PrepareRow): PrepareRow
  }
  
  def lift[T](v: T): T = macro LiftingMacro.lift[T]
  
  @compileTimeOnly(NonQuotedException.message)
  def lift[T](v: T, e: Encoder[T]): T = NonQuotedException()

  case class MappedEncoding[I, O](f: I => O)

  def mappedEncoding[I, O](f: I => O) = MappedEncoding(f)

  implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], decoder: Decoder[I]): Decoder[O] =
    new Decoder[O] {
      def apply(index: Int, row: ResultRow) =
        mapped.f(decoder(index, row))
    }

  implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], encoder: Encoder[O]): Encoder[I] =
    new Encoder[I] {
      def apply(index: Int, value: I, row: PrepareRow) =
        encoder(index, mapped.f(value), row)
    }

  implicit def wrappedTypeDecoder[T <: WrappedType] =
    MappedEncoding[T, T#Type](_.value)
}
