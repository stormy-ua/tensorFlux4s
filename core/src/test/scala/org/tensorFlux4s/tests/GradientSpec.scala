package org.tensorFlux4s
package tests

import org.scalatest.{FlatSpec, Matchers}
import cats._
import cats.implicits._
import cats.syntax.either._
import cats.syntax.EitherSyntax._
import cats.syntax._

class GradientSpec extends FlatSpec with Matchers with TensorMatchers {

  import dsl._
  import computation.graph._
  import gradient._

  it should "calculate gradient of sum" in {

    def expr[F[_] : Monad, T](alg: Aux[F, T])(variables: Map[String, T]): F[T] = {
      import alg._
      import dsl.implicits._

      implicit val aux = alg.aux

      def var_(name: String) = variable(name)(variables(name))
      def const_(name: String) = constant(variables(name))

      var_("x") + var_("y")
    }

    implicit val alg = bindings.nd4j.algebra
    val falg = CompGraphAlgebra(alg)

    import alg._

    val mgrads = for {
      x  <- cons(Seq(1, 2, 3, 4), 2, 2)
      y  <- cons(Seq(4, 5, 6, 7), 2, 2)
      vars = Map("x" -> x, "y" -> y)
      g  <- grad(expr(falg))(vars)
      ng <- numericalGrad(expr(falg))(vars)
    } yield (g, ng)

    mgrads match {
      case Left(e) => fail(e)
      case Right((grads, ngrads)) =>
        grads.keys should be (ngrads.keys)
        grads.keys.foreach { v =>
          assertAreEqual(grads(v), ngrads(v))
        }
    }
  }

}
