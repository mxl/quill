package io.getquill.context.jdbc

import java.math.{ BigDecimal => JBigDecimal }
import java.sql.{ Timestamp, Date => SqlDate }
import java.time.LocalDate
import java.util.Date

import io.getquill.context.sql.encoding.ArrayEncoding
import io.getquill.util.Messages.fail

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait ArrayDecoders extends ArrayEncoding {
  self: JdbcContext[_, _] =>

  implicit def arrayStringDecoder[Col <: Seq[String]](implicit bf: CanBuildFrom[Nothing, String, Col]): RawDecoder[Col] = arrayRawDecoder[String, Col]

  implicit def arrayBigDecimalDecoder[Col <: Seq[BigDecimal]](implicit bf: CBF[BigDecimal, Col]): RawDecoder[Col] = arrayDecoder[JBigDecimal, BigDecimal, Col](BigDecimal.apply)

  implicit def arrayBooleanDecoder[Col <: Seq[Boolean]](implicit bf: CBF[Boolean, Col]): RawDecoder[Col] = arrayRawDecoder[Boolean, Col]

  implicit def arrayByteDecoder[Col <: Seq[Byte]](implicit bf: CBF[Byte, Col]): RawDecoder[Col] = arrayRawDecoder[Byte, Col]

  implicit def arrayShortDecoder[Col <: Seq[Short]](implicit bf: CBF[Short, Col]): RawDecoder[Col] = arrayRawDecoder[Short, Col]

  implicit def arrayIntDecoder[Col <: Seq[Int]](implicit bf: CBF[Int, Col]): RawDecoder[Col] = arrayRawDecoder[Int, Col]

  implicit def arrayLongDecoder[Col <: Seq[Long]](implicit bf: CBF[Long, Col]): RawDecoder[Col] = arrayRawDecoder[Long, Col]

  implicit def arrayFloatDecoder[Col <: Seq[Float]](implicit bf: CBF[Float, Col]): RawDecoder[Col] = arrayRawDecoder[Float, Col]

  implicit def arrayDoubleDecoder[Col <: Seq[Double]](implicit bf: CBF[Double, Col]): RawDecoder[Col] = arrayRawDecoder[Double, Col]

  implicit def arrayDateDecoder[Col <: Seq[Date]](implicit bf: CBF[Date, Col]): RawDecoder[Col] = arrayRawDecoder[Date, Col]

  implicit def arrayTimestampDecoder[Col <: Seq[Timestamp]](implicit bf: CBF[Timestamp, Col]): RawDecoder[Col] = arrayRawDecoder[Timestamp, Col]

  implicit def arrayLocalDateDecoder[Col <: Seq[LocalDate]](implicit bf: CBF[LocalDate, Col]): RawDecoder[Col] = arrayDecoder[SqlDate, LocalDate, Col](_.toLocalDate)

  /**
   * Generic encoder for JDBC arrays.
   *
   * @param mapper retrieved raw types fro JDBC array may be mapped via this mapper to satisfy encoder type
   * @param bf     builder factory is needed to create instances of decoder's collection
   * @tparam I   raw type retrieved form JDBC array
   * @tparam O   mapped type fulfilled in decoder's collection
   * @tparam Col seq type
   * @return JDBC array decoder
   */
  def arrayDecoder[I, O, Col <: Seq[O]](mapper: I => O)(implicit bf: CanBuildFrom[Nothing, O, Col], tag: ClassTag[I]): RawDecoder[Col] =
    rawDecoder[Col]((row: ResultRow) => (idx: Index) => {
      val arr = row.getArray(idx)
      if (arr == null) bf().result()
      else arr.getArray.asInstanceOf[Array[AnyRef]].foldLeft(bf()) {
        case (b, x: I)                => b += mapper(x)
        case (b, x: java.lang.Number) => b += mapper(x.asInstanceOf[I])
        case (_, x) =>
          fail(s"Retrieved ${x.getClass.getCanonicalName} type from JDBC array, but expected $tag. Re-check your decoder implementation")
      }.result()
    })

  /**
   * Creates JDBC array decoder for type `T` which is already supported by database as array element.
   *
   * @param bf builder factory is needed to create instances of decoder's collection
   * @tparam T   element type
   * @tparam Col seq type
   * @return JDBC array decoder
   */
  def arrayRawDecoder[T: ClassTag, Col <: Seq[T]](implicit bf: CanBuildFrom[Nothing, T, Col]): RawDecoder[Col] =
    arrayDecoder[T, T, Col](identity)
}
