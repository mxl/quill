package test

import io.getquill.MirrorContext

object Test extends App {
  val ctx = new MirrorContext
  import ctx._
  
  case class Test(a: Int)

  run(query[Test].delete)
}