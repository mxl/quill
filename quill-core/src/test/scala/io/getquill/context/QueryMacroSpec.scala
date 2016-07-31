package io.getquill.context

import io.getquill.Spec
import io.getquill.context.mirror.Row
import io.getquill.testContext
import io.getquill.testContext._
import io.getquill.norm.BetaReduction

class QueryMacroSpec extends Spec {

  "runs non-binded query" in {
    val q = quote {
      qr1.map(_.i)
    }
    testContext.run(q).ast mustEqual q.ast
  }

  "runs query with liftings" - {
    "one" in {
      val q = quote {
        qr1.filter(t => t.i == lift(1)).map(t => t.i)
      }
      val r = testContext.run(q)
      r.ast mustEqual q.ast
      r.binds mustEqual Row(1)
    }
    "two" in {
      val q = quote {
        qr1.filter(t => t.i == lift(1) && t.s == lift("a")).map(t => t.i)
      }
      val r = testContext.run(q)
      r.ast mustEqual q.ast
      r.binds mustEqual Row(1, "a")
    }
    "nested" in {
      val c = quote {
        (t: TestEntity) => t.i == lift(1)
      }
      val q = quote {
        qr1.filter(t => c(t) && t.s == lift("a")).map(t => t.i)
      }
      val r = testContext.run(q)
      r.ast mustEqual BetaReduction(q.ast)
      r.binds mustEqual Row(1, "a")
    }
  }
}
