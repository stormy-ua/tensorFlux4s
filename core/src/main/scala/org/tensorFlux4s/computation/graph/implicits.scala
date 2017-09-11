package org.tensorFlux4s.computation.graph

import scala.annotation.tailrec

object implicits {

  implicit class Extensions[V, E](val g: AcyclicGraph[V, E]) extends AnyVal {

    def value: V = g match {
      case AcyclicGraph.empty    => ???
      case LeafVertex(v)         => v
      case UnaryVertex(v, _)     => v
      case BinaryVertex(v, _, _) => v
    }

    def traversePreOrder[S](s: S)(f: (S, AcyclicGraph[V, E]) => S): S = {

      @tailrec
      def go(s: S, gr: List[AcyclicGraph[V, E]]): S = gr match {
        case Nil => s
        case AcyclicGraph.empty :: xs => s
        case (vt @ LeafVertex(_)) :: xs => go(f(s, vt), xs)
        case (vt @ UnaryVertex(_, (_, in))) :: xs =>  go(f(s, vt), in :: xs)
        case (vt @ BinaryVertex(_, (_, in1), (_, in2))) :: xs => go(f(s, vt), in1 :: in2 :: xs)
      }

      go(s, List(g))
    }

  }

}