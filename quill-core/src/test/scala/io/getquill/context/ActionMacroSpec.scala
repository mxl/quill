package io.getquill.context

import io.getquill.Spec
import io.getquill.testContext
import io.getquill.testContext._
import io.getquill.context.mirror.Row

class ActionMacroSpec extends Spec {

  "runs non-batched action" - {
    "normal" in {
      val q = quote {
        qr1.delete
      }
      testContext.run(q).ast mustEqual q.ast
    }
    "inline" in {
      def q(i: Int) =
        testContext.run(qr1.filter(_.i == lift(i)).update(_.i -> 0))
      q(1).bind mustEqual Row(1)
    }
  }

  "runs with returning value" in {
    val q = quote {
      qr1.insert(_.i -> 1).returning(_.l)
    }
    val r = testContext.run(q)
    r.ast mustEqual q.ast
    r.returning mustEqual Some("l")
  }

  //  "runs batched action" - {
  //    "one param" in {
  //      val q = quote {
  //        (p1: Int) => qr1.insert(_.i -> p1)
  //      }
  //      val r = testContext.run(q)(List(1, 2))
  //      r.ast mustEqual q.ast.body
  //      r.bindList mustEqual List(Row(1), Row(2))
  //    }
  //    "two params" in {
  //      val q = quote {
  //        (p1: Int, p2: String) => qr1.insert(_.i -> p1, _.s -> p2)
  //      }
  //      val r = testContext.run(q)(List((1, "a"), (2, "b")))
  //      r.ast mustEqual q.ast.body
  //      r.bindList mustEqual List(Row(1, "a"), Row(2, "b"))
  //    }
  //    "with in-place binding" in {
  //      val q = quote { (i: Int) => (s: Int) => qr1.update(_.i -> i, _.s -> s)
  //      }
  //      val v = 1
  //      val r = testContext.run(q(lift(v)))(List(1, 2))
  //      q.ast.body match {
  //        case f: Function => r.ast mustEqual r.ast
  //        case other       => fail
  //      }
  //      r.bindList mustEqual List(Row(1, v), Row(2, v))
  //    }
  //  }
  //
}
