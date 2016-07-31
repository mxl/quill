package io.getquill.dsl

import scala.reflect.macros.whitebox.Context
import io.getquill.lifting.LiftingMacro

private[dsl] class Macro(val c: Context) extends LiftingMacro