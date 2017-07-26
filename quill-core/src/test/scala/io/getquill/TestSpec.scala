package io.getquill
import testContext._
import language.reflectiveCalls

class TestSpec extends Spec {
  "test" in {
    case class TestEntity(s: String, i: Int)

    case class Builder(build: Quoted[EntityQuery[TestEntity]])

    def q1 = quote {
      (q: EntityQuery[TestEntity]) => Builder(q.filter(_.s == "s")).build
    }
    println(q1.ast)

    val m1 = testContext.run(q1(query[TestEntity]))

    println(m1.string)

    val q2 = quote {
      new {
        def apply(q: EntityQuery[TestEntity]) =
          Builder(q.filter(_.s == "s")).build
      }
    }
    println(q2.ast)

    val q3 = q2(query[TestEntity])

    println(q3.ast)

    val m2 = testContext.run(q3)

    println(m2.string)
  }
  //  "test2" in {
  //    trait SomeTrait {
  //      val s: String
  //      val i: Int
  //    }
  //
  //    case class SomeClass1(s: String, i: Int) extends SomeTrait
  //    case class SomeClass2(s: String, i: Int) extends SomeTrait
  //
  //    case class Builder[T](build: Quoted[EntityQuery[T]])
  //
  //    def q1[T <: SomeTrait] = quote {
  //      (q: EntityQuery[T]) => Builder(q.filter(_.s == "s")).build
  //    }
  //
  //    val m1 = testContext.run(q1(query[SomeClass1]))
  //
  //    println(m1.string)
  //
  //    val q2 = quote {
  //      new {
  //        def apply[T <: SomeTrait](q: EntityQuery[T]) =
  //          Builder(q.filter(_.s == "s")).build
  //      }
  //    }
  //
  //    val m2 = testContext.run(q2(query[SomeClass2]))
  //
  //    println(m2.string)
  //  }
}
