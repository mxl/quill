package io.getquill.context

import scala.reflect.ClassTag

import io.getquill.Spec
import io.getquill.testContext
import io.getquill.testContext.Action
import io.getquill.testContext.InfixInterpolator
import io.getquill.testContext.Query
import io.getquill.testContext.Quoted
import io.getquill.testContext.TestEntity
import io.getquill.testContext.lift
import io.getquill.testContext.qr1
import io.getquill.testContext.query
import io.getquill.testContext.quote
import io.getquill.testContext.unquote
import mirror.Row

class ContextMacroSpec extends Spec {

  "runs actions" - {
    "non-parametrized" - {
      "normal" in {
        val q = quote {
          qr1.delete
        }
        testContext.run(q).ast mustEqual q.ast
      }
      "infix" in {
        val q = quote {
          infix"STRING".as[Action[TestEntity, Long]]
        }
        testContext.run(q).ast mustEqual q.ast
      }
      "dynamic" in {
        val q: Quoted[Action[TestEntity, Long]] = quote {
          qr1.delete
        }
        testContext.run(q).ast mustEqual q.ast
      }
      "dynamic type param" in {
        def test[T: ClassTag] = quote(query[T].delete)
        val r = testContext.run(test[TestEntity])
        r.ast.toString mustEqual "query[TestEntity].delete"
      }
    }
    "parametrized" - {
      "normal" in {
        val q = quote {
          qr1.filter(t => t.s == lift("a")).delete
        }
        val r = testContext.run(q)
        r.ast.toString mustEqual "query[TestEntity].filter(t => t.s == lift(a)).delete"
        r.bind mustEqual Row("a")
      }
      "infix" in {
        val q = quote {
          infix"t = ${lift("a")}".as[Action[TestEntity, Long]]
        }
        val r = testContext.run(q)
        r.ast.toString mustEqual s"""infix"t = $${lift(a)}""""
        r.bind mustEqual Row("a")
      }
      "dynamic" in {
        val q: Quoted[Action[TestEntity, Long]] = quote {
          infix"t = ${lift("a")}".as[Action[TestEntity, Long]]
        }
        val r = testContext.run(q)
        r.ast.toString mustEqual s"""infix"t = $${lift(a)}""""
        r.bind mustEqual Row("a")
      }
      "dynamic type param" in {
        import language.reflectiveCalls
        def test[T <: { def i: Int }: ClassTag] = quote {
          query[T].filter(t => t.i == lift(1)).delete
        }
        val r = testContext.run(test[TestEntity])
        r.ast.toString mustEqual "query[TestEntity].filter(t => t.i == lift(1)).delete"
        r.bind mustEqual Row(1)
      }
    }
  }

  "runs queries" - {
    "non-parametrized" - {
      "normal" in {
        val q = quote {
          qr1.map(t => t.s)
        }
        testContext.run(q).ast mustEqual q.ast
      }
      "infix" in {
        val q = quote {
          infix"STRING".as[Query[TestEntity]].map(t => t.s)
        }
        testContext.run(q).ast mustEqual q.ast
      }
      "dynamic" in {
        val q: Quoted[Query[String]] = quote {
          qr1.map(t => t.s)
        }
        testContext.run(q).ast mustEqual q.ast
      }
      "dynamic type param" in {
        def test[T: ClassTag] = quote(query[T])
        val r = testContext.run(test[TestEntity])
        r.ast.toString mustEqual "query[TestEntity].map(x => (x.s, x.i, x.l, x.o))"
      }
    }
    "parametrized" - {
      "normal" in {
        val q = quote {
          qr1.filter(t => t.s == lift("a"))
        }
        val r = testContext.run(q)
        r.ast.toString mustEqual "query[TestEntity].filter(t => t.s == lift(a)).map(t => (t.s, t.i, t.l, t.o))"
        r.binds mustEqual Row("a")
      }

      "wrapped" in {
        case class Entity(x: WrappedEncodable)
        val q = quote {
          query[Entity].filter(t => t.x == lift(WrappedEncodable(1)))
        }
        val r = testContext.run(q)
        r.ast.toString mustEqual "query[Entity].filter(t => t.x == lift(1)).map(t => t.x)"
        r.binds mustEqual Row(1)
      }
      "infix" in {
        val q = quote {
          infix"SELECT ${lift("a")}".as[Query[String]]
        }
        val r = testContext.run(q)
        r.ast.toString mustEqual s"""infix"SELECT $${lift(a)}".map(x => x)"""
        r.binds mustEqual Row("a")
      }
      "dynamic" in {
        val q: Quoted[Query[TestEntity]] = quote {
          qr1.filter(t => t.s == lift("a"))
        }
        val r = testContext.run(q)
        r.ast.toString mustEqual "query[TestEntity].filter(t => t.s == lift(a)).map(t => (t.s, t.i, t.l, t.o))"
        r.binds mustEqual Row("a")
      }
      "dynamic type param" in {
        def test[T: ClassTag] = quote {
          query[T].map(t => lift(1))
        }
        val r = testContext.run(test[TestEntity])
        r.ast.toString mustEqual "query[TestEntity].map(t => lift(1))"
        r.binds mustEqual Row(1)
      }
    }
    "aggregated" in {
      val q = quote {
        qr1.map(t => t.i).max
      }
      testContext.run(q).ast mustEqual q.ast
    }
  }

  "can't be used as a var" in {
    var db = testContext
    "db.run(qr1)" mustNot compile
  }

  "fails if there's a free variable" in {
    val q = {
      val i = 1
      quote {
        qr1.filter(_.i == i)
      }
    }
    "testContext.run(q)" mustNot compile
  }
}
