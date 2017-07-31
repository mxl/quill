package io.getquill.context.cassandra.encoding

import java.util.{ Date, UUID }

import com.datastax.driver.core.LocalDate
import io.getquill.context.cassandra.CassandraSessionContext

trait Decoders extends CollectionDecoders {
  this: CassandraSessionContext[_] =>

  case class CassandraDecoder[T](decoder: BaseDecoder[T]) extends BaseDecoder[T] {
    override def apply(index: Index, row: ResultRow) =
      decoder(index, row)
  }

  def rawDecoder[T](f: ResultRow => Index => T): RawDecoder[T] =
    (index, row) => {
      if (row.isNull(index) && !row.getColumnDefinitions.getType(index).isCollection)
        None
      else Some(f(row)(index))
    }

  implicit val stringDecoder: RawDecoder[String] = rawDecoder(_.getString)
  implicit val bigDecimalDecoder: RawDecoder[BigDecimal] =
    rawDecoder(row => index => row.getDecimal(index))
  implicit val booleanDecoder: RawDecoder[Boolean] = rawDecoder(_.getBool)
  implicit val intDecoder: RawDecoder[Int] = rawDecoder(_.getInt)
  implicit val longDecoder: RawDecoder[Long] = rawDecoder(_.getLong)
  implicit val floatDecoder: RawDecoder[Float] = rawDecoder(_.getFloat)
  implicit val doubleDecoder: RawDecoder[Double] = rawDecoder(_.getDouble)
  implicit val byteArrayDecoder: RawDecoder[Array[Byte]] =
    rawDecoder(row => index => {
      val bb = row.getBytes(index)
      val b = new Array[Byte](bb.remaining())
      bb.get(b)
      b
    })
  implicit val uuidDecoder: RawDecoder[UUID] = rawDecoder(_.getUUID)
  implicit val dateDecoder: RawDecoder[Date] = rawDecoder(_.getTimestamp)
  implicit val localDateDecoder: RawDecoder[LocalDate] = rawDecoder(_.getDate)
}
