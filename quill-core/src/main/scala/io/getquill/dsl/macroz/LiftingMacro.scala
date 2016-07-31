package io.getquill.dsl.macroz
import io.getquill.util.Messages._
import scala.reflect.macros.whitebox.{ Context => MacroContext }
import scala.reflect.macros.whitebox.{Context => MacroContext}
import scala.reflect.macros.whitebox.{Context => MacroContext}

trait LiftingMacro {
  val c: MacroContext
  import c.universe._

  def lift[T](v: Expr[T])(implicit t: WeakTypeTag[T]): Tree =
    liftTree(v.tree)

  protected def liftTree[T](v: Tree)(implicit t: WeakTypeTag[T]): Tree = {
    regularEncoder[T]
      .orElse(anyValEncoder[T]) match {
        case Some(enc) => q"${c.prefix}.lift($v, $enc)"
        case None =>
          t.tpe.baseType(c.symbolOf[Product]) match {
            case NoType =>
              c.fail(s"Can't find encoder for type '${t.tpe}'")
            case _ =>
              q"${c.prefix}.liftCaseClass($v)"
          }
      }
  }

  private def regularEncoder[T](implicit t: WeakTypeTag[T]): Option[Tree] =
    c.typecheck(
      q"implicitly[${c.prefix}.Encoder[$t]]",
      silent = true
    ) match {
        case EmptyTree => None
        case tree      => Some(tree)
      }

  private def anyValEncoder[T](implicit t: WeakTypeTag[T]): Option[Tree] =
    t.tpe.baseType(c.symbolOf[AnyVal]) match {
      case NoType => None
      case _ =>
        caseClassConstructor(t.tpe).map(_.paramLists.flatten).collect {
          case param :: Nil =>
            regularEncoder(c.WeakTypeTag(param.typeSignature)) match {
              case Some(encoder) =>
                c.typecheck(q"""
                  ${c.prefix}.mappedEncoder(
                    ${c.prefix}.MappedEncoding((v: $t) => v.${param.name.toTermName}), 
                    $encoder
                  )
                """)
              case None =>
                c.fail(s"Can't encode the '${t.tpe}' because there's no encoder for '$param'.")
            }
        }
    }

  private def caseClassConstructor(t: Type) =
    t.members.collect {
      case m: MethodSymbol if (m.isPrimaryConstructor) => m
    }.headOption
}
