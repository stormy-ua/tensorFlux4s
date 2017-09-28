package org.tensorFlux4s

import cats.{Monad, MonadError, Semigroup}
import cats.implicits._
import dsl._
import computation.graph._

package object gradient {

  def forward[F[_], T](comp: Computation[F, T])
                      (variables: Map[String, T])
                      (implicit m: Monad[F], me: MonadError[F, Throwable], alg: Aux[F, T]): F[T] = {
    import computation.graph.implicits._

    for {
      f <- comp(liftConstants(variables))
    } yield f.value.value
  }

  def grad[F[_], T](comp: Computation[F, T])
                   (variables: Map[String, T])
                   (implicit m: Monad[F], me: MonadError[F, Throwable], alg: Aux[F, T]): F[Map[String, T]] = {
    for {
      f <- comp(liftConstants(variables))
      b =  backward(f)(alg)
      r <- b.collect {
        case (LeafVertex(Payload(Op.Variable, _, Some(name))), v @ _) => v.map(name -> _)
      }.toList.sequence
    } yield r.toMap
  }

  def numericalGrad[F[_], T](comp: Computation[F, T])
                            (variables: Map[String, T])
                            (implicit m: Monad[F], me: MonadError[F, Throwable], alg: Aux[F, T]): F[Map[String, T]] = {

    import alg._

    val dx = 1E-3

    def calcGrad(varName: String) = {
      val x = variables(varName)

      for {
        s <- shape(x)
        dt <- fill(dx, s: _*)
        fv <- forward(comp)(variables)
        shifts <- (0 until s.product).map {
          i =>
            for {
              v <- get(x, i)
              shifted <- set(x, i, v + dx)
            } yield shifted
        }.toList.sequence
        fshifts <- shifts.map(sh => forward(comp)(variables.updated(varName, sh))).sequence
        diff <- fshifts.map(sh => subtract(sh, fv)).sequence
        dfshifts <- diff.zip(0 until s.product).map {
          case (sh, i) => get(sh, i).map(_/dx)
        }.sequence
        grad <- cons(dfshifts, s:_*)
      } yield grad
    }

    variables.keys.map { k => calcGrad(k).map(g => k -> g) }.toList.sequence.map(_.toMap)
  }


  def gradDescentStep[F[_], T](comp: Computation[F, T])
                              (variables: Map[String, T])
                              (implicit m: Monad[F], me: MonadError[F, Throwable], alg: Aux[F, T]): F[Map[String, T]] = {

    import dsl.implicits._

    val learningRate = -1E-5

    implicit val semi = new Semigroup[F[alg.T]] {

      def combine(x: F[alg.T], y: F[alg.T]): F[alg.T] = for {
        xt <- x
        yt <- y
        dt <- alg.fill(learningRate, 1, 1)
        dy <- yt * dt
        s <- xt + dy
      } yield s

    }

    for {
      grads  <- grad(comp)(variables)
      fgrads = grads.mapValues(_.pure[F])
      fvars  = variables.mapValues(_.pure[F])
      merged = fvars |+| fgrads
      res    <- merged.map { case(k, v) => v.map(k -> _) }.toList.sequence
    } yield res.toMap
  }

}
