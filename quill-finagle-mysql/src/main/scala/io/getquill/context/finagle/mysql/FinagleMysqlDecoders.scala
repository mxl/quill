package io.getquill.context.finagle.mysql

import java.time.{LocalDate, LocalDateTime}
import java.util.{Date, UUID}

import com.twitter.finagle.mysql._
import io.getquill.FinagleMysqlContext
import io.getquill.util.Messages.fail

import scala.reflect.{ClassTag, classTag}

trait FinagleMysqlDecoders {
  this: FinagleMysqlContext[_] =>

  def rawDecoder[T: ClassTag](f: PartialFunction[Value, T]): RawDecoder[T] =
    (index, row) => row.values(index) match {
      case NullValue => None
      case value if f.isDefinedAt(value) => Some(f(value))
      case value => fail(s"Value '$value' can't be decoded to '${classTag[T].runtimeClass}'")
    }

  implicit val stringDecoder: RawDecoder[String] =
    rawDecoder[String] {
      case StringValue(v) => v
    }
  
  implicit val bigDecimalDecoder: RawDecoder[BigDecimal] =
    rawDecoder[BigDecimal] {
      case BigDecimalValue(v) => v
    }
  implicit val booleanDecoder: RawDecoder[Boolean] =
    rawDecoder[Boolean] {
      case ByteValue(v) => v == (1: Byte)
      case ShortValue(v) => v == (1: Short)
      case IntValue(v) => v == 1
      case LongValue(v) => v == 1L
      case v: RawValue if v.typ == Type.Bit => v.bytes.head == (1: Byte)
    }
  implicit val byteDecoder: RawDecoder[Byte] =
    rawDecoder[Byte] {
      case ByteValue(v) => v
      case ShortValue(v) => v.toByte
    }
  implicit val shortDecoder: RawDecoder[Short] =
    rawDecoder[Short] {
      case ShortValue(v) => v
    }
  implicit val intDecoder: RawDecoder[Int] =
    rawDecoder[Int] {
      case IntValue(v) => v
      case LongValue(v) => v.toInt
    }
  implicit val longDecoder: RawDecoder[Long] =
    rawDecoder[Long] {
      case IntValue(v) => v.toLong
      case LongValue(v) => v
    }
  implicit val floatDecoder: RawDecoder[Float] =
    rawDecoder[Float] {
      case FloatValue(v) => v
    }
  implicit val doubleDecoder: RawDecoder[Double] =
    rawDecoder[Double] {
      case DoubleValue(v) => v
    }
  implicit val byteArrayDecoder: RawDecoder[Array[Byte]] =
    rawDecoder[Array[Byte]] {
      case v: RawValue => v.bytes
    }
  implicit val dateDecoder: RawDecoder[Date] =
    rawDecoder[Date] {
      case `timestampValue`(v) => new Date(v.getTime)
      case DateValue(d) => new Date(d.getTime)
    }

  implicit val localDateDecoder: RawDecoder[LocalDate] = rawDecoder[LocalDate] {
    case `timestampValue`(v) => v.toLocalDateTime.toLocalDate
    case DateValue(d) => d.toLocalDate
  }

  implicit val localDateTimeDecoder: RawDecoder[LocalDateTime] = rawDecoder[LocalDateTime] {
    case `timestampValue`(v) => v.toInstant.atZone(extractionTimeZone.toZoneId).toLocalDateTime
  }

  implicit val uuidDecoder: RawDecoder[UUID] = mappedRawDecoder(MappedEncoding(UUID.fromString), stringDecoder)
}
