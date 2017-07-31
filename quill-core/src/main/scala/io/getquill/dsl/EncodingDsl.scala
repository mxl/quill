package io.getquill.dsl

import io.getquill.quotation.NonQuotedException
import io.getquill.util.Messages.fail

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.language.higherKinds

trait LowPriorityImplicits {
  this: EncodingDsl =>

  implicit def anyValEncoder[T <: AnyVal]: Encoder[T] = macro EncodingDslMacro.anyValEncoder[T]

  implicit def anyValDecoder[T <: AnyVal]: Decoder[T] = macro EncodingDslMacro.anyValDecoder[T]
}

trait EncodingDsl extends LowPriorityImplicits {
  this: CoreDsl =>

  type PrepareRow
  type ResultRow

  type Index = Int

  type BaseEncoder[T] = (Index, T, PrepareRow) => PrepareRow

  type Encoder[T] <: BaseEncoder[T]

  type BaseDecoder[T] = (Index, ResultRow) => T

  type Decoder[T] <: BaseDecoder[T]

  type RawDecoded[_]

  type RawDecoder[T] = Decoder[RawDecoded[T]]

  /* ************************************************************************** */

  def lift[T](v: T): T = macro EncodingDslMacro.lift[T]

  @compileTimeOnly(NonQuotedException.message)
  def liftScalar[T](v: T)(implicit e: Encoder[T]): T = NonQuotedException()

  @compileTimeOnly(NonQuotedException.message)
  def liftCaseClass[T](v: T): T = NonQuotedException()

  /* ************************************************************************** */

  def liftQuery[U[_] <: Traversable[_], T](v: U[T]): Query[T] = macro EncodingDslMacro.liftQuery[T]

  @compileTimeOnly(NonQuotedException.message)
  def liftQueryScalar[U[_] <: Traversable[_], T](v: U[T])(implicit e: Encoder[T]): Query[T] = NonQuotedException()

  @compileTimeOnly(NonQuotedException.message)
  def liftQueryCaseClass[U[_] <: Traversable[_], T](v: U[T]): Query[T] = NonQuotedException()

  /* ************************************************************************** */

  type MappedEncoding[I, O] = io.getquill.MappedEncoding[I, O]
  val MappedEncoding = io.getquill.MappedEncoding

  implicit def anyValMappedEncoder[I <: AnyVal, O](implicit mapped: MappedEncoding[I, O], encoder: Encoder[O]): Encoder[I] = mappedEncoder

  implicit def anyValMappedDecoder[I, O <: AnyVal](implicit mapped: MappedEncoding[I, O], decoder: Decoder[I]): Decoder[O] = mappedDecoder

  implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], encoder: Encoder[O]): Encoder[I]

  implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], decoder: Decoder[I]): Decoder[O]

  implicit def decoder[T](implicit d: RawDecoder[T]): Decoder[T]

  implicit def mappedRawDecoder[I, O](implicit mapped: MappedEncoding[I, O], d: RawDecoder[I]): RawDecoder[O]

  protected def mappedBaseEncoder[I, O](mapped: MappedEncoding[I, O], encoder: BaseEncoder[O]): BaseEncoder[I] =
    (index, value, row) => encoder(index, mapped.f(value), row)

  protected def mappedBaseDecoder[I, O](mapped: MappedEncoding[I, O], decoder: BaseDecoder[I]): BaseDecoder[O] =
    (index, row) => mapped.f(decoder(index, row))
}

trait PlainEncoderDsl {
  this: EncodingDsl =>

  type Encoder[T] = BaseEncoder[T]

  implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], encoder: Encoder[O]): Encoder[I] =
    (index, value, row) => encoder(index, mapped.f(value), row)
}

trait PlainDecoderDsl {
  this: EncodingDsl =>

  type Decoder[T] = BaseDecoder[T]

  implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], d: Decoder[I]): Decoder[O] =
    (index, row) => mapped.f(d(index, row))
}

trait IdentityRawDecoderDsl {
  this: EncodingDsl =>

  type RawDecoded[T] = T

  implicit def decoder[T](implicit d: RawDecoder[T]): Decoder[T] = d
}

trait PlainIdentityRawDecoderDsl extends IdentityRawDecoderDsl with PlainDecoderDsl {
  this: EncodingDsl =>

  implicit def mappedRawDecoder[I, O](implicit mapped: MappedEncoding[I, O], d: RawDecoder[I]): RawDecoder[O] =
    (index, row) => mapped.f(d(index, row))
}

trait OptionRawDecoderDsl {
  this: EncodingDsl =>

  type RawDecoded[T] = Option[T]

  def baseDecoder[T](implicit d: RawDecoder[T]): BaseDecoder[T] =
    (index, row) => d(index, row) match {
      case None => fail(s"Expected column at index $index to be defined but it was empty.")
      case Some(v) => v
    }
}

trait PlainOptionRawDecoderDsl extends OptionRawDecoderDsl with PlainDecoderDsl {
  this: EncodingDsl =>

  implicit def decoder[T](implicit d: RawDecoder[T]): Decoder[T] = baseDecoder(d)

  implicit def mappedRawDecoder[I, O](implicit mapped: MappedEncoding[I, O], d: RawDecoder[I]): RawDecoder[O] =
    (index, row) => d(index, row).map(mapped.f)
}
