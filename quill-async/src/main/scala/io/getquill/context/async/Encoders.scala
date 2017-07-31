package io.getquill.context.async

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.util.Date

import org.joda.time.{LocalDate => JodaLocalDate, LocalDateTime => JodaLocalDateTime}

trait Encoders {
  this: AsyncContext[_, _, _] =>

  def encoder[T](f: T => Any = identity[T]): Encoder[T] =
    (_, value, row) => row :+ f(value)

  implicit def optionEncoder[T](implicit d: Encoder[T]): Encoder[Option[T]] =
    (index, value, row) => value match {
      case None => nullEncoder(index, null, row)
      case Some(v) => d(index, v, row)
    }

  private[this] val nullEncoder: Encoder[Null] = encoder[Null]()

  implicit val stringEncoder: Encoder[String] = encoder[String]()
  implicit val bigDecimalEncoder: Encoder[BigDecimal] = encoder[BigDecimal]()
  implicit val booleanEncoder: Encoder[Boolean] = encoder[Boolean]()
  implicit val byteEncoder: Encoder[Byte] = encoder[Byte]()
  implicit val shortEncoder: Encoder[Short] = encoder[Short]()
  implicit val intEncoder: Encoder[Int] = encoder[Int]()
  implicit val longEncoder: Encoder[Long] = encoder[Long]()
  implicit val floatEncoder: Encoder[Float] = encoder[Float]()
  implicit val doubleEncoder: Encoder[Double] = encoder[Double]()
  implicit val byteArrayEncoder: Encoder[Array[Byte]] = encoder[Array[Byte]]()
  implicit val jodaLocalDateEncoder: Encoder[JodaLocalDate] = encoder[JodaLocalDate]()
  implicit val jodaLocalDateTimeEncoder: Encoder[JodaLocalDateTime] = encoder[JodaLocalDateTime]()
  implicit val dateEncoder: Encoder[Date] = encoder[Date](d => new JodaLocalDateTime(d))

  implicit val encodeLocalDate: MappedEncoding[LocalDate, JodaLocalDate] =
    MappedEncoding(ld => new JodaLocalDate(ld.getYear, ld.getMonthValue, ld.getDayOfMonth))

  implicit val encodeLocalDateTime: MappedEncoding[LocalDateTime, JodaLocalDateTime] =
    MappedEncoding(ldt => new JodaLocalDateTime(ldt.atZone(ZoneId.systemDefault()).toInstant.toEpochMilli))

  implicit val localDateEncoder: Encoder[LocalDate] = mappedEncoder(encodeLocalDate, jodaLocalDateEncoder)
}
