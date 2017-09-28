package org.tensorFlux4s
package examples

import cats.{Monad, MonadError}
import cats.syntax.either._
import cats.syntax.EitherSyntax._
import cats.syntax._
import cats.implicits._
import dsl._
import org.tensorFlux4s.computation.graph.{CompGraph, CompGraphAlgebra, Computation}

import scala.language.postfixOps

object ArtificialNeuron {

  def neuron[F[_] : Monad, T](alg: Aux[F, T])(variables: Map[String, T]): F[T] = {
    import alg._
    import dsl.implicits._

    def var_(name: String) = variable(name)(variables(name))
    def const_(name: String) = constant(variables(name))

    for {
      output  <- (var_("x") dot var_("w")) + var_("b")
      zs      <- zeroes(4, 1)
      cost    <- (output maximum zs) - const_("y")
      r       <- (cost * cost) \+/
    } yield r
  }

  def main(args: Array[String]): Unit = {

    import computation.graph._
    import gradient._

    implicit val alg = bindings.nd4j.algebra
    val falg = CompGraphAlgebra(alg)

    import alg._

    val result = for {
      w  <- fill(0, 2, 1)
      x  <- cons(Seq(
        0.0, 0.0,
        1.0, 1.0,
        9.0, 9.0,
        10.0, 10.0), 4, 2)
      y  <- cons(Seq(0, 0, 1, 1), 4, 1)
      b  <- fill(0.1, 4, 1)
      vars = Map("w" -> w, "b" -> b, "x" -> x, "y" -> y)
      f  <- forward(neuron(falg))(vars)

      _  = println(s"f = $f")
      grads <- grad(neuron(falg))(vars)
      _  = grads.foreach { case (k, v) => println(s"analytical df/d$k = ${grads(k)}") }
      numGrads <- numericalGrad(neuron(falg))(vars)
      _  = grads.foreach { case (k, v) => println(s"numerical df/d$k = ${numGrads(k)}") }

      /*vars2  <- gradDescentStep(neuron(falg))(vars)
      _  = println(s"vars2 = $vars2")
      f2  <- forward(neuron(falg))(vars2)
      _  = println(s"f2 = $f2")*/

      vars2  <- (0 until 1000).foldLeft(vars.pure[bindings.nd4j.F]) { (v, index) =>
        for {
          vs <- v
          uvs  <- gradDescentStep(neuron(falg))(vs)
          //_  = println(s"vars2 = $uvs")
          f2  <- forward(neuron(falg))(uvs)
          _  = println(s"f2 = $f2")
        } yield uvs
      }

      _ = println(s"optimized vars = $vars2")

      ty <- alg.dot(x, vars2("w"))
      tyb <- alg.sum(ty, vars2("b"))

      _ = println(s"test y = $tyb")
    } yield ()

    println(s"result = $result")


  }

}
