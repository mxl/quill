package test

import io.getquill.{CassandraAsyncContext, SnakeCase}


case class Test(
                  timestamp: Int,
                  coordinate: Map[String,Int],
                  assetIds: Array[Int],
                  buildingId: Int,
                  floorId: Int,
                  floorRegionId: Int,
                  floorPartId: Int,
                  triggerId: Int,
                  resolved: Boolean,
                  claimedBy: String,
                  resolveLog: String
                )

class vacuum {
  lazy val ctx = new CassandraAsyncContext[SnakeCase]("ctx")
  import ctx._

  val insertQuote = quote(query[Test].insert)
  ctx.run(insertQuote)(Test(
    1469804954,
    Map("x" -> 100, "y" -> 100),
    Array[Int](100,200),
    1,
    3,
    1001,
    1112,
    42,
    false,
    "",
    "{\"userID\": 234234,\"comment\": \"The matter was investigated and everyone got cupcakes.\"},{\"userID\": 234234, \"comment\": \"The matter was investigated and everyone got cupcakes.\"}"
  ))
}