package io.getquill.context.jdbc

import java.sql.Timestamp
import java.time.{ LocalDate, LocalDateTime }
import java.{ sql, util }
import java.util.Calendar

import scala.math.BigDecimal.javaBigDecimal2bigDecimal

trait Decoders {
  this: JdbcContext[_, _] =>

  def rawDecoder[T](f: ResultRow => Int => T): RawDecoder[T] =
    new RawDecoder[T] {
      def apply(index: Int, row: ResultRow): RawDecoded[T] = {
        val v = f(row)(index + 1)
        if (row.wasNull()) {
          None
        } else {
          Some(v)
        }
      }
    }

  implicit val stringDecoder: RawDecoder[String] = rawDecoder(_.getString)
  implicit val bigDecimalDecoder: RawDecoder[BigDecimal] = rawDecoder[BigDecimal](row => index => row.getBigDecimal(index))
  implicit val booleanDecoder: RawDecoder[Boolean] = rawDecoder(_.getBoolean)
  implicit val byteDecoder: RawDecoder[Byte] = rawDecoder(_.getByte)
  implicit val shortDecoder: RawDecoder[Short] = rawDecoder(_.getShort)
  implicit val intDecoder: RawDecoder[Int] = rawDecoder(_.getInt)
  implicit val longDecoder: RawDecoder[Long] = rawDecoder(_.getLong)
  implicit val floatDecoder: RawDecoder[Float] = rawDecoder(_.getFloat)
  implicit val doubleDecoder: RawDecoder[Double] = rawDecoder(_.getDouble)
  implicit val byteArrayDecoder: RawDecoder[Array[Byte]] = rawDecoder(_.getBytes)
  implicit val sqlTimestampDecoder: RawDecoder[Timestamp] =
    rawDecoder(row => index => row.getTimestamp(index, Calendar.getInstance(dateTimeZone)))
  implicit val dateDecoder: RawDecoder[util.Date] =
    mappedRawDecoder(MappedEncoding[Timestamp, util.Date](v => new util.Date(v.getTime)), sqlTimestampDecoder)
  implicit val sqlDateDecoder: RawDecoder[sql.Date] =
    rawDecoder(row => index => row.getDate(index, Calendar.getInstance(dateTimeZone)))
  implicit val localDateDecoder: RawDecoder[LocalDate] =
    mappedRawDecoder(MappedEncoding[sql.Date, LocalDate](_.toLocalDate), sqlDateDecoder)
  implicit val localDateTimeDecoder: RawDecoder[LocalDateTime] =
    mappedRawDecoder(MappedEncoding[Timestamp, LocalDateTime](_.toLocalDateTime), sqlTimestampDecoder)
}
