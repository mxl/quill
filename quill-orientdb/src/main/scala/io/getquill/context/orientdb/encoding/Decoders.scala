package io.getquill.context.orientdb.encoding

import java.util.Date

import io.getquill.context.orientdb.OrientDBSessionContext
import io.getquill.util.Messages.fail

import scala.reflect.{ClassTag, classTag}

trait Decoders extends CollectionDecoders {
  this: OrientDBSessionContext[_] =>

  def rawDecoder[S, T: ClassTag](f: PartialFunction[S, T] = PartialFunction[T, T](identity),
    d: BaseDecoder[S] = (index, row) => row.field[S](row.fieldNames()(index))): RawDecoder[T] =
    (index, row) =>
      if (row.fieldValues()(index) == null) {
        None
      } else {
        val value = d(index, row)
        if (f.isDefinedAt(value)) {
          Some(f(value))
        } else {
          fail(s"Value '$value' at index $index can't be decoded to '${classTag[T].runtimeClass}'")
        }
      }

  implicit val stringDecoder: RawDecoder[String] = rawDecoder()
  implicit val doubleDecoder: RawDecoder[Double] = rawDecoder()
  implicit val bigDecimalDecoder: RawDecoder[BigDecimal] = rawDecoder[Double, BigDecimal](PartialFunction(BigDecimal.apply))
  implicit val booleanDecoder: RawDecoder[Boolean] = rawDecoder()
  implicit val intDecoder: RawDecoder[Int] = rawDecoder()
  implicit val shortDecoder: RawDecoder[Short] = rawDecoder()
  implicit val longDecoder: RawDecoder[Long] = rawDecoder[Any, Long] {
    case v: Int => v.toLong
    case v: Long => v
  }
  implicit val floatDecoder: RawDecoder[Float] = rawDecoder[Double, Float](PartialFunction(_.toFloat))
  implicit val byteArrayDecoder: RawDecoder[Array[Byte]] = rawDecoder()
  implicit val dateDecoder: RawDecoder[Date] = rawDecoder()
}