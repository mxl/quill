package io.getquill.context

import scala.concurrent.duration.DurationInt
import scala.reflect.api.Types
import scala.reflect.macros.whitebox.{ Context => MacroContext }
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import io.getquill._
import io.getquill.util.Cache
import io.getquill.util.Messages.RichContext

object ProbeStatement {

  private val cache = new Cache[Types#Type, Context[_]]

  def apply[Statement](statement: Statement, c: MacroContext) = {
    import c.universe.{ Try => _, _ }

    def resolveContext(tpe: Type) =
      tpe match {
        case tpe if (tpe <:< c.weakTypeOf[QueryProbing]) =>
          loadContext(tpe) match {
            case Success(context) =>
              Some(context)
            case Failure(ex) =>
              c.error(s"Can't load the context of type '$tpe' for compile-time query probing. Reason: '$ex'")
              None
          }
        case other =>
          None
      }

    def loadContext(tpe: Type): Try[Context[Statement]] =
      Try {
        tpe match {
          case tpe if (tpe.erasure <:< c.weakTypeOf[Singleton]) =>
            val cls = Class.forName(tpe.typeSymbol.fullName + "$")
            val field = cls.getField("MODULE$")
            field.get(cls)
          case tpe =>
            Class.forName(tpe.typeSymbol.fullName).newInstance
        }
      }.asInstanceOf[Try[Context[Statement]]]

    val tpe = c.prefix.tree.tpe

    cache
      .getOrElseUpdate(tpe, resolveContext(tpe), 30.seconds)
      .map(_.asInstanceOf[Context[Statement]].probe(statement))
      .map {
        case Success(_) =>
        case Failure(ex) =>
          c.error(s"Query probing failed. Reason: '$ex'")
      }
  }
}
