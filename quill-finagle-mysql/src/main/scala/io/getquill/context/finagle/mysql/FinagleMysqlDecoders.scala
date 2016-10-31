package io.getquill.context.finagle.mysql

import java.time.{ LocalDate, LocalDateTime }
import java.util.Date

import scala.reflect.ClassTag
import scala.reflect.classTag
import com.twitter.finagle.mysql._
import io.getquill.util.Messages.fail
import io.getquill.FinagleMysqlContext

trait FinagleMysqlDecoders {
  this: FinagleMysqlContext[_] =>

  protected val timestampValue =
    new TimestampValue(
      dateTimezone,
      dateTimezone
    )

  def decoder[T: ClassTag](f: PartialFunction[Value, T]): Decoder[T] =
    new Decoder[T] {
      def apply(index: Int, row: Row) = {
        val value = row.values(index)
        f.lift(value).getOrElse(fail(s"Value '$value' can't be decoded to '${classTag[T].runtimeClass}'"))
      }
    }

  implicit def optionDecoder[T](implicit d: Decoder[T]): Decoder[Option[T]] =
    new Decoder[Option[T]] {
      def apply(index: Int, row: Row) = {
        row.values(index) match {
          case NullValue => None
          case other     => Some(d(index, row))
        }
      }
    }

  implicit val stringDecoder: Decoder[String] =
    decoder[String] {
      case StringValue(v) => v
    }
  implicit val bigDecimalDecoder: Decoder[BigDecimal] =
    decoder[BigDecimal] {
      case BigDecimalValue(v) => v
    }
  implicit val booleanDecoder: Decoder[Boolean] =
    decoder[Boolean] {
      case ByteValue(v)                     => v == (1: Byte)
      case ShortValue(v)                    => v == (1: Short)
      case IntValue(v)                      => v == 1
      case LongValue(v)                     => v == 1L
      case v: RawValue if v.typ == Type.Bit => v.bytes.head == (1: Byte)
    }
  implicit val byteDecoder: Decoder[Byte] =
    decoder[Byte] {
      case ByteValue(v)  => v
      case ShortValue(v) => v.toByte
    }
  implicit val shortDecoder: Decoder[Short] =
    decoder[Short] {
      case ShortValue(v) => v
    }
  implicit val intDecoder: Decoder[Int] =
    decoder[Int] {
      case IntValue(v)  => v
      case LongValue(v) => v.toInt
    }
  implicit val longDecoder: Decoder[Long] =
    decoder[Long] {
      case IntValue(v)  => v.toLong
      case LongValue(v) => v
    }
  implicit val floatDecoder: Decoder[Float] =
    decoder[Float] {
      case FloatValue(v) => v
    }
  implicit val doubleDecoder: Decoder[Double] =
    decoder[Double] {
      case DoubleValue(v) => v
    }
  implicit val byteArrayDecoder: Decoder[Array[Byte]] =
    decoder[Array[Byte]] {
      case v: RawValue => v.bytes
    }
  implicit val dateDecoder: Decoder[Date] =
    decoder[Date] {
      case `timestampValue`(v) => new Date(v.getTime)
    }

  implicit val localDateDecoder: Decoder[LocalDate] = decoder[LocalDate] {
    case `timestampValue`(v) => v.toLocalDateTime.toLocalDate
    case DateValue(d)        => d.toLocalDate
  }

  implicit val localDateTimeDecoder: Decoder[LocalDateTime] = decoder[LocalDateTime] {
    case `timestampValue`(v) => v.toLocalDateTime
  }
}
