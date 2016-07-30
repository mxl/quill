package io.getquill.dsl

import io.getquill.WrappedType
import scala.annotation.compileTimeOnly
import io.getquill.quotation.NonQuotedException

trait EncodingDsl {

  type Row
  type Result

  trait Decoder[+T] {
    def apply(index: Int, row: Result): T
  }

  trait Encoder[-T] {
    def apply(index: Int, value: T, row: Row): Row
  }
  
  @compileTimeOnly(NonQuotedException.message)
  def lift[T](v: T)(e: Encoder[T]): T = NonQuotedException()

  case class MappedEncoding[I, O](f: I => O)

  def mappedEncoding[I, O](f: I => O) = MappedEncoding(f)

  implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], decoder: Decoder[I]): Decoder[O] =
    new Decoder[O] {
      def apply(index: Int, row: Result) =
        mapped.f(decoder(index, row))
    }

  implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], encoder: Encoder[O]): Encoder[I] =
    new Encoder[I] {
      def apply(index: Int, value: I, row: Row) =
        encoder(index, mapped.f(value), row)
    }

  implicit def wrappedTypeDecoder[T <: WrappedType] =
    MappedEncoding[T, T#Type](_.value)
}
