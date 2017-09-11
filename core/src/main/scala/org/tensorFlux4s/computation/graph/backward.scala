package org.tensorFlux4s
package computation
package graph

import cats.{Monad, Semigroup}
import cats.implicits._
import dsl._
import implicits._

object backward {

  private def addGrad[F[_] : Monad, T](alg: Aux[F, T])
                                      (grads: Map[CompGraph[T], List[F[T]]], v: CompGraph[T], gs: (CompGraph[T], F[T] => F[T])*)
  = {
    implicit val semi = new Semigroup[F[alg.T]] {

      def combine(x: F[alg.T], y: F[alg.T]): F[alg.T] = for {
        xt <- x
        yt <- y
        s <- alg.sum(xt, yt)
      } yield s

    }

    gs.foldLeft(grads) { case (s, (node, g)) =>
      val gds = grads.getOrElse(v, List(alg.shape(node.value.value).flatMap(sh => alg.ones(sh: _*)))).head
      val ng = g(gds)
      Map(node -> List(ng)) |+| s
    }
  }

  def apply[F[_] : Monad, T](g: CompGraph[T])(alg: Aux[F, T]): Map[CompGraph[T], F[T]] = {

    implicit val semi = new Semigroup[F[alg.T]] {

      def combine(x: F[alg.T], y: F[alg.T]): F[alg.T] = for {
        xt <- x
        yt <- y
        s <- alg.sum(xt, yt)
      } yield s

    }

    g.traversePreOrder(Map.empty[CompGraph[T], List[F[T]]]) { (grads, vertex) =>

      import alg._

      vertex match {
        case vrt @ UnaryVertex(Payload(Op.ReduceSum, v, _), (_, in)) =>
          addGrad(alg)(grads, vrt, in -> identity)

        case vrt @ BinaryVertex(Payload(Op.Sum, v, _), (_, in1), (_, in2)) =>
          addGrad(alg)(grads, vrt, in1 -> identity, in2 -> identity)

        case vrt @ BinaryVertex(Payload(Op.Multiply, v, _), (_, in1), (_, in2)) =>
          addGrad(alg)(grads, vrt,
            in1 -> { grad => grad.flatMap(multiply(in2.value.value, _)) },
            in2 -> { grad => grad.flatMap(multiply(in1.value.value, _)) })

        case vrt @ BinaryVertex(Payload(Op.GreaterThan, v, _), (_, in1), (_, in2)) =>
          addGrad(alg)(grads, vrt,
            in1 -> { grad =>
              for {
                g  <- grad
                gt <- greaterThan(in1.value.value, in2.value.value)
                rg <- multiply(g, gt)
              } yield rg
            },
            in2 -> { grad =>
              for {
                g  <- grad
                gt <- greaterThan(in2.value.value, in1.value.value)
                rg <- multiply(g, gt)
              } yield rg
            })

        case vrt @ BinaryVertex(Payload(Op.Maximum, v, _), (_, in1), (_, in2)) =>
          addGrad(alg)(grads, vrt,
            in1 -> { grad =>
              for {
                g  <- grad
                gt <- greaterThan(in1.value.value, in2.value.value)
                rg <- multiply(g, gt)
              } yield rg
            },
            in2 -> { grad =>
              for {
                g  <- grad
                gt <- greaterThan(in2.value.value, in1.value.value)
                rg <- multiply(g, gt)
              } yield rg
            })

        case vrt @ BinaryVertex(Payload(Op.Subtract, v, _), (_, in1), (_, in2)) =>
          addGrad(alg)(grads, vrt, in1 -> identity, in2 -> { grad =>
            for {
              g  <- grad
              s  <- shape(g)
              m1 <- fill(-1, s:_*)
              rg <- multiply(g, m1)
            } yield rg
          })

        case vrt @ BinaryVertex(Payload(Op.Dot, v, _), (_, in1), (_, in2)) =>
          addGrad(alg)(grads, vrt,
            in1 -> {
              vrtGrad1 => for {
                g <- vrtGrad1
                in2t <- transpose(in2.value.value)
                dp  <- dot(g, in2t)
              } yield dp
            },
            in2 -> {
              vrtGrad2 => for {
                g <- vrtGrad2
                in1t <- transpose(in1.value.value)
                dp  <- dot(in1t, g)
              } yield dp
            })


        case vrt @ LeafVertex(_) => grads

        case _ => ???
      }

    }.mapValues(gs => gs.reduce(_ |+| _))

  }

}