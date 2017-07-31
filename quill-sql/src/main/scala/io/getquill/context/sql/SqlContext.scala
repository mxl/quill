package io.getquill.context.sql

import java.time.LocalDate
import java.util.{Date, UUID}

import io.getquill.NamingStrategy
import io.getquill.context.Context
import io.getquill.context.sql.dsl.SqlDsl
import io.getquill.idiom.{Idiom => BaseIdiom}

trait SqlContext[Idiom <: BaseIdiom, Naming <: NamingStrategy]
  extends Context[Idiom, Naming]
    with SqlDsl {

  implicit val stringDecoder: RawDecoder[String]
  implicit val bigDecimalDecoder: RawDecoder[BigDecimal]
  implicit val booleanDecoder: RawDecoder[Boolean]
  implicit val byteDecoder: RawDecoder[Byte]
  implicit val shortDecoder: RawDecoder[Short]
  implicit val intDecoder: RawDecoder[Int]
  implicit val longDecoder: RawDecoder[Long]
  implicit val floatDecoder: RawDecoder[Float]
  implicit val doubleDecoder: RawDecoder[Double]
  implicit val byteArrayDecoder: RawDecoder[Array[Byte]]
  implicit val dateDecoder: RawDecoder[Date]
  implicit val localDateDecoder: RawDecoder[LocalDate]
  implicit val uuidDecoder: RawDecoder[UUID]

  implicit def optionEncoder[T](implicit d: Encoder[T]): Encoder[Option[T]]

  implicit val stringEncoder: Encoder[String]
  implicit val bigDecimalEncoder: Encoder[BigDecimal]
  implicit val booleanEncoder: Encoder[Boolean]
  implicit val byteEncoder: Encoder[Byte]
  implicit val shortEncoder: Encoder[Short]
  implicit val intEncoder: Encoder[Int]
  implicit val longEncoder: Encoder[Long]
  implicit val floatEncoder: Encoder[Float]
  implicit val doubleEncoder: Encoder[Double]
  implicit val byteArrayEncoder: Encoder[Array[Byte]]
  implicit val dateEncoder: Encoder[Date]
  implicit val localDateEncoder: Encoder[LocalDate]
  implicit val uuidEncoder: Encoder[UUID]
}
