package io.getquill.context.mirror

import java.time.LocalDate
import java.util.{ Date, UUID }

import io.getquill.MirrorContext

import scala.reflect.ClassTag

trait MirrorDecoders {
  this: MirrorContext[_, _] =>

  def rawDecoder[T: ClassTag]: RawDecoder[T] = (index: Index, row: ResultRow) =>
    row[T](index)

  def decoderUnsafe[T]: Decoder[T] = (index: Index, row: ResultRow) => row.data(index).asInstanceOf[T]

  implicit def optionDecoder[T](implicit d: Decoder[T]): Decoder[Option[T]] =
    (index: Index, row: ResultRow) =>
      row[Option[Any]](index) match {
        case Some(v) => Some(d(0, Row(v)))
        case None    => None
      }

  implicit val stringDecoder: RawDecoder[String] = rawDecoder[String]
  implicit val bigDecimalDecoder: RawDecoder[BigDecimal] = rawDecoder[BigDecimal]
  implicit val booleanDecoder: RawDecoder[Boolean] = rawDecoder[Boolean]
  implicit val byteDecoder: RawDecoder[Byte] = rawDecoder[Byte]
  implicit val shortDecoder: RawDecoder[Short] = rawDecoder[Short]
  implicit val intDecoder: RawDecoder[Int] = rawDecoder[Int]
  implicit val longDecoder: RawDecoder[Long] = rawDecoder[Long]
  implicit val floatDecoder: RawDecoder[Float] = rawDecoder[Float]
  implicit val doubleDecoder: RawDecoder[Double] = rawDecoder[Double]
  implicit val byteArrayDecoder: RawDecoder[Array[Byte]] = rawDecoder[Array[Byte]]
  implicit val dateDecoder: RawDecoder[Date] = rawDecoder[Date]
  implicit val localDateDecoder: RawDecoder[LocalDate] = rawDecoder[LocalDate]
  implicit val uuidDecoder: RawDecoder[UUID] = rawDecoder[UUID]
}
