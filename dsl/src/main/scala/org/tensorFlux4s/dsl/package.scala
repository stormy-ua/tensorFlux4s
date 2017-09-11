package org.tensorFlux4s

import cats._
import cats.implicits._

package object dsl {

  trait TensorAlgebra[F[_]] {

    type T

    val aux: Aux[F, T]

    def pure(t: T): F[T]

    def cons(values: Seq[Double], shape: Int*): F[T]

    def ones(shape: Int*): F[T]

    def zeroes(shape: Int*): F[T]

    def fill(v: Double, shape: Int*): F[T]

    def set(t: T, index: Int, value: Double): F[T]

    def get(t: T, index: Int): F[Double]

    def sum(t1: T, t2: T): F[T]

    def exp(t: T): F[T]

    def subtract(t1: T, t2: T): F[T]

    def multiply(t1: T, t2: T): F[T]

    def dot(t1: T, t2: T): F[T]

    def divide(t1: T, t2: T): F[T]

    def greaterThan(t1: T, t2: T): F[T]

    def maximum(t1: T, t2: T): F[T]

    def reduceSum(t: T): F[T]

    def length(t: T): F[Int]

    def shape(t: T): F[Array[Int]]

    def map(t: T)(f: Double => Double): F[T]

    def transpose(t: T): F[T]

    def variable(name: String)(t: T): F[T]

    def constant(t: T): F[T] = pure(t)

    def liftUnary(f: T => F[T], t: F[T])(implicit m: Monad[F]): F[T] = t.flatMap(f(_))

    def liftBinary(f: (T, T) => F[T], t1: F[T], t2: F[T])(implicit m: Monad[F]): F[T] = for {
      x1 <- t1
      x2 <- t2
      s  <- f(x1, x2)
    } yield s

    def sum(t1: F[T], t2: F[T])(implicit m: Monad[F]): F[T] =
      liftBinary(sum, t1, t2)

    def multiply(t1: F[T], t2: F[T])(implicit m: Monad[F]): F[T] =
      liftBinary(multiply, t1, t2)

    def divide(t1: F[T], t2: F[T])(implicit m: Monad[F]): F[T] =
      liftBinary(divide, t1, t2)

    def dot(t1: F[T], t2: F[T])(implicit m: Monad[F]): F[T] =
      liftBinary(dot, t1, t2)

    def subtract(t1: F[T], t2: F[T])(implicit m: Monad[F]): F[T] =
      liftBinary(subtract, t1, t2)

    def maximum(t1: F[T], t2: F[T])(implicit m: Monad[F]): F[T] =
      liftBinary(maximum, t1, t2)

    def reduceSum(t: F[T])(implicit m: Monad[F]): F[T] =
      liftUnary(reduceSum, t)
  }

  type Aux[F[_], A] = TensorAlgebra[F] { type T = A }

}
