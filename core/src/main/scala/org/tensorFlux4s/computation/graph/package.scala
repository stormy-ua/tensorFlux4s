package org.tensorFlux4s.computation

import cats.implicits._
import org.tensorFlux4s.dsl.Aux

package object graph {

  type Edge[+V, +E] = (E, AcyclicGraph[V, E])

  type CompGraph[A] = AcyclicGraph[Payload[A], Unit]

  type Computation[F[_], T] = Map[String, CompGraph[T]] => F[CompGraph[T]]

  type Computation2[F[_], T] = Aux[F, T] => Map[String, T] => F[T]

  def liftConstant[T](name: String)(t: T): CompGraph[T] =
    LeafVertex(Payload(Op.Constant, t, name.some))

  def liftConstants[T](vars: Map[String, T]): Map[String, CompGraph[T]] =
    vars.map { case (k, v) => k -> liftConstant(k)(v) }

}
