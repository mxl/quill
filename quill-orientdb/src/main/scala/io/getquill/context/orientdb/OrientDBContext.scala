package io.getquill.context.orientdb

import java.util.Date

import io.getquill.NamingStrategy
import io.getquill.context.Context
import io.getquill.context.orientdb.dsl.OrientDBDsl
import io.getquill.dsl.{PlainEncoderDsl, PlainOptionRawDecoderDsl}

trait OrientDBContext[Naming <: NamingStrategy]
  extends Context[OrientDBIdiom, Naming]
  with PlainEncoderDsl
  with PlainOptionRawDecoderDsl
  with OrientDBDsl {

  implicit def optionEncoder[T](implicit d: Encoder[T]): Encoder[Option[T]]
  implicit val stringEncoder: Encoder[String]
  implicit val bigDecimalEncoder: Encoder[BigDecimal]
  implicit val booleanEncoder: Encoder[Boolean]
  implicit val shortEncoder: Encoder[Short]
  implicit val intEncoder: Encoder[Int]
  implicit val longEncoder: Encoder[Long]
  implicit val floatEncoder: Encoder[Float]
  implicit val doubleEncoder: Encoder[Double]
  implicit val dateEncoder: Encoder[Date]
  implicit val byteArrayEncoder: Encoder[Array[Byte]]
  implicit def listEncoder[T]: Encoder[List[T]]
  implicit def setEncoder[T]: Encoder[Set[T]]
  implicit def mapEncoder[K, V]: Encoder[Map[K, V]]

  implicit val stringDecoder: RawDecoder[String]
  implicit val doubleDecoder: RawDecoder[Double]
  implicit val bigDecimalDecoder: RawDecoder[BigDecimal]
  implicit val booleanDecoder: RawDecoder[Boolean]
  implicit val shortDecoder: RawDecoder[Short]
  implicit val intDecoder: RawDecoder[Int]
  implicit val longDecoder: RawDecoder[Long]
  implicit val floatDecoder: RawDecoder[Float]
  implicit val byteArrayDecoder: RawDecoder[Array[Byte]]
  implicit val dateDecoder: RawDecoder[Date]
  implicit def listDecoder[T]: RawDecoder[List[T]]
  implicit def setDecoder[T]: RawDecoder[Set[T]]
  implicit def mapDecoder[K, V]: RawDecoder[Map[K, V]]
}