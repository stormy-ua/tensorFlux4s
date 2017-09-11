package org.tensorFlux4s.computation.graph

sealed trait Op

object Op {

  case object Sum         extends Op
  case object Subtract    extends Op
  case object Multiply    extends Op
  case object Divide      extends Op
  case object GreaterThan extends Op
  case object Maximum     extends Op
  case object Dot         extends Op
  case object ReduceSum   extends Op
  case object Constant    extends Op
  case object Variable    extends Op

}
