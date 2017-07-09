package io.getquill

import java.util.Date

import io.getquill.ast.Ast
import io.getquill.testContext._
import io.getquill.idiom.StatementInterpolator._

class TestSpec extends Spec {

  "test" in {
    import MirrorIdiom._

    case class TestEntity(id: Int, date: Date)

    val q = quote {
      query[TestEntity].filter(t => t.date >= lift(new Date()))
    }

    val s1 = stmt"${(q.ast: Ast).token}"
    val s2 = stmt"""querySchema("TestEntity").filter(t => infix"$${t.date} >= $${lift(new java.util.Date())}")
    print(s1.tokens)
    s1.tokens.size mustEqual s2.tokens.size
    s1 mustEqual s2

  }
}
