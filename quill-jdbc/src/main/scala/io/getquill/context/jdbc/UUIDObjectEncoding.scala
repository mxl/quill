package io.getquill.context.jdbc

import java.sql.Types
import java.util.UUID

trait UUIDObjectEncoding {
  this: JdbcContext[_, _] =>
  implicit val uuidEncoder: Encoder[UUID] = encoder(Types.OTHER, (index, value, row) => row.setObject(index, value, Types.OTHER))
  implicit val uuidDecoder: RawDecoder[UUID] = rawDecoder(row => index => UUID.fromString(row.getObject(index).toString))
}
