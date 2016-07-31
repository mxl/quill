package io.getquill.lifting

import io.getquill.util.Messages._
import scala.reflect.macros.whitebox.{ Context => MacroContext }

class LiftingMacro(val c: MacroContext) {
  import c.universe._

  def lift[T](v: Expr[T])(implicit t: WeakTypeTag[T]): Tree = {
    regularEncoder[T]
      .orElse(anyValEncoder[T]) match {
        case None      => c.fail(s"Can't find encoder for type '${t.tpe}'")
        case Some(enc) => q"${c.prefix}.lift($v, $enc)"
      }
  }

  def regularEncoder[T](implicit t: WeakTypeTag[T]): Option[Tree] = { 
    c.info(q"implictly[${c.prefix}.Encoder[$t]]".toString)
    c.typecheck(
      q"implicitly[${c.prefix}.Encoder[$t]]",
      silent = true) match {
        case EmptyTree => None
        case tree      => Some(tree)
      }
  }

  def anyValEncoder[T](implicit t: WeakTypeTag[T]): Option[Tree] =
    caseClassConstructor(t.tpe).map(_.paramLists.flatten).collect {
      case param :: Nil =>
        c.typecheck(q"""
          ${c.prefix}.mappedEncoder(
            ${c.prefix}.MappedEncoding((v: $t) => v.${param.name.toTermName}), 
            implicitly[${c.prefix}.Encoder[${param.typeSignature}]]
          )
        """)
    }

  private def caseClassConstructor(t: Type) =
    t.members.collect {
      case m: MethodSymbol if (m.isPrimaryConstructor) => m
    }.headOption
}
