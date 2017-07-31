package io.getquill.context.async

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.util.Date

import io.getquill.util.Messages.fail
import org.joda.time.{LocalDate => JodaLocalDate, LocalDateTime => JodaLocalDateTime}

import scala.reflect.{ClassTag, classTag}

trait Decoders {
  this: AsyncContext[_, _, _] =>

  def rawDecoder[T: ClassTag](f: PartialFunction[Any, T] = PartialFunction.empty): RawDecoder[T] =
    (index: Index, row: ResultRow) => {
      row(index) match {
        case null => None
        case value: T => Some(value)
        case value if f.isDefinedAt(value) => Some(f(value))
        case value =>
          fail(s"Value '$value' at index $index can't be decoded to '${classTag[T].runtimeClass}'")
      }
    }

  def numericDecoder[T, U](f: (U, Numeric[U]) => T): RawDecoder[T] = {
    def decode[O](u: O)(implicit n: Numeric[O]): T = f(u, n)

    rawDecoder {
      case v: Byte => decode(v)
      case v: Short => decode(v)
      case v: Int => decode(v)
      case v: Long => decode(v)
      case v: Float => decode(v)
      case v: Double => decode(v)
      case v: BigDecimal => decode(v)
      case other =>
        fail(s"Value $other is not numeric")
    }
  }

  implicit val stringDecoder: RawDecoder[String] = rawDecoder[String]()

  implicit val bigDecimalDecoder: RawDecoder[BigDecimal] =
    numericDecoder((v, n) => BigDecimal(n.toDouble(v)))

  implicit val booleanDecoder: RawDecoder[Boolean] =
    rawDecoder[Boolean] {
      case v: Byte => v == (1: Byte)
      case v: Short => v == (1: Short)
      case v: Int => v == 1
      case v: Long => v == 1L
    }

  implicit val byteDecoder: RawDecoder[Byte] =
    rawDecoder[Byte] {
      case v: Short => v.toByte
    }

  implicit val shortDecoder: RawDecoder[Short] =
    rawDecoder[Short] {
      case v: Byte => v.toShort
    }

  implicit val intDecoder: RawDecoder[Int] =
    numericDecoder((v, n) => n.toInt(v))

  implicit val longDecoder: RawDecoder[Long] =
    numericDecoder((v, n) => n.toLong(v))

  implicit val floatDecoder: RawDecoder[Float] =
    numericDecoder((v, n) => n.toFloat(v))

  implicit val doubleDecoder: RawDecoder[Double] =
    numericDecoder((v, n) => n.toDouble(v))

  implicit val byteArrayDecoder: RawDecoder[Array[Byte]] = rawDecoder[Array[Byte]]()

  implicit val jodaLocalDateDecoder: RawDecoder[JodaLocalDate] = rawDecoder[JodaLocalDate] {
    case localDate: JodaLocalDate => localDate
  }

  implicit val jodaLocalDateTimeDecoder: RawDecoder[JodaLocalDateTime] = rawDecoder[JodaLocalDateTime] {
    case localDateTime: JodaLocalDateTime => localDateTime
  }

  implicit val dateDecoder: RawDecoder[Date] = rawDecoder[Date] {
    case localDateTime: JodaLocalDateTime => localDateTime.toDate
    case localDate: JodaLocalDate => localDate.toDate
  }

  implicit val decodeLocalDate: MappedEncoding[JodaLocalDate, LocalDate] =
    MappedEncoding(jld => LocalDate.of(jld.getYear, jld.getMonthOfYear, jld.getDayOfMonth))

  implicit val decodeLocalDateTime: MappedEncoding[JodaLocalDateTime, LocalDateTime] =
    MappedEncoding(jldt => LocalDateTime.ofInstant(jldt.toDate.toInstant, ZoneId.systemDefault()))

  implicit val localDateDecoder: RawDecoder[LocalDate] = mappedRawDecoder(decodeLocalDate, jodaLocalDateDecoder)
}
