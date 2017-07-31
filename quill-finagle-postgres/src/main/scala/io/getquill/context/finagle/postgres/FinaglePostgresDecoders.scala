package io.getquill.context.finagle.postgres

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.util.{Date, UUID}

import com.twitter.finagle.postgres.values.ValueDecoder
import io.getquill.FinaglePostgresContext
import io.getquill.util.Messages.fail

import scala.reflect.{ClassTag, classTag}

trait FinaglePostgresDecoders {
  this: FinaglePostgresContext[_] =>

  import ValueDecoder._

  def rawDecoder[T: ClassTag](f: PartialFunction[Any, T]): RawDecoder[T] =
    (index, row) => row.getAnyOption(index).map {
      case v: T => v
      case v if f.isDefinedAt(v) => f(v)
      case v => fail(s"Cannot decode value $v at index $index to ${classTag[T]}")
    }

  implicit def decoderDirectly[T: ClassTag](implicit vd: ValueDecoder[T]): RawDecoder[T] =
    (index, row) => row.getOption[T](index)

  implicit val stringDecoder: RawDecoder[String] = decoderDirectly[String]
  implicit val bigDecimalDecoder: RawDecoder[BigDecimal] = decoderDirectly[BigDecimal]
  implicit val booleanDecoder: RawDecoder[Boolean] = decoderDirectly[Boolean]
  implicit val byteDecoder: RawDecoder[Byte] = rawDecoder[Byte] {
    case v: Short => v.toByte
  }
  implicit val shortDecoder: RawDecoder[Short] = decoderDirectly[Short]
  implicit val intDecoder: RawDecoder[Int] =
    rawDecoder[Int] {
      case v: Int => v
      case v: Long => v.toInt
    }
  implicit val longDecoder: RawDecoder[Long] =
    rawDecoder[Long] {
      case v: Int => v.toLong
      case v: Long => v
    }
  implicit val floatDecoder: RawDecoder[Float] = rawDecoder[Float] {
    case v: Double => v.toFloat
    case v: Float => v
  }
  implicit val doubleDecoder: RawDecoder[Double] = rawDecoder[Double] {
    case v: Double => v
    case v: Float => v.toDouble
  }
  implicit val byteArrayDecoder: RawDecoder[Array[Byte]] = decoderDirectly[Array[Byte]]
  implicit val dateDecoder: RawDecoder[Date] =
    rawDecoder[Date] {
      case d: LocalDateTime => Date.from(d.atZone(ZoneId.systemDefault()).toInstant);
    }
  implicit val localDateDecoder: RawDecoder[LocalDate] = rawDecoder[LocalDate] {
    case d: LocalDateTime => d.toLocalDate
    case d: LocalDate => d
  }
  implicit val localDateTimeDecoder: RawDecoder[LocalDateTime] = rawDecoder[LocalDateTime] {
    case d: LocalDateTime => d
    case d: LocalDate => d.atStartOfDay()
  }

  implicit val uuidDecoder: RawDecoder[UUID] = decoderDirectly[UUID]
}