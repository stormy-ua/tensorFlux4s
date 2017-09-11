package org.tensorFlux4s.computation.graph

sealed trait AcyclicGraph[+V, +E]

case class LeafVertex[+V, +E](value: V)                                     extends AcyclicGraph[V, E]

case class UnaryVertex[+V, +E](value: V, in: Edge[V, E])                    extends AcyclicGraph[V, E]

case class BinaryVertex[+V, +E](value: V, in1: Edge[V, E], in2: Edge[V, E]) extends AcyclicGraph[V, E]

object AcyclicGraph {

  case object empty extends AcyclicGraph[Nothing, Nothing]

}