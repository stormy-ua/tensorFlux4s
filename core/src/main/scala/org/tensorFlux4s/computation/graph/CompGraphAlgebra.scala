package org.tensorFlux4s
package computation
package graph

import cats.{Applicative, MonadError, Monad}
import cats.implicits._
import dsl._
import implicits._

object CompGraphAlgebra {

  def apply[F[_]](alg: TensorAlgebra[F])(implicit m: Applicative[F], me: MonadError[F, Throwable]) = new StubTensorAlgebra[F] {

    type TP = alg.T
    type T = CompGraph[alg.T]

    implicit val aux: Aux[F, T] = this

    private def liftUnary(op: Op, opF: TP => F[TP], t: T) : F[T] =
      opF(t.value.value).map { x => UnaryVertex(Payload(op, x), () -> t) }

    private def liftBinary(op: Op, opF: (TP, TP) => F[TP], t1: T, t2: T) : F[T] =
      opF(t1.value.value, t2.value.value).map { x =>
        BinaryVertex(Payload(op, x), () -> t1, () -> t2)
      }

    override def pure(t: T): F[T] = t.pure[F]

    override def cons(values: Seq[Double], shape: Int*): F[T] =
      alg.cons(values, shape:_*).map(x => LeafVertex(Payload(Op.Constant, x)))

    override def ones(shape: Int*): F[T] =
      alg.ones(shape:_*).map(x => LeafVertex(Payload(Op.Constant, x)))

    override def zeroes(shape: Int*): F[T] =
      alg.zeroes(shape:_*).map(x => LeafVertex(Payload(Op.Constant, x)))

    override def sum(t1: T, t2: T): F[T] =
      liftBinary(Op.Sum, (x, y) => alg.sum(x, y), t1, t2)

    override def multiply(t1: T, t2: T): F[T] =
      liftBinary(Op.Multiply, (x, y) => alg.multiply(x, y), t1, t2)

    override def divide(t1: T, t2: T): F[T] =
      liftBinary(Op.Divide, (x, y) => alg.divide(x, y), t1, t2)

    override def subtract(t1: T, t2: T): F[T] =
      liftBinary(Op.Subtract, (x, y) => alg.subtract(x, y), t1, t2)

    override def greaterThan(t1: T, t2: T): F[T] =
      liftBinary(Op.GreaterThan, (x, y) => alg.greaterThan(x, y), t1, t2)

    override def maximum(t1: T, t2: T): F[T] =
      liftBinary(Op.Maximum, (x, y) => alg.maximum(x, y), t1, t2)

    override def dot(t1: T, t2: T): F[T] =
      liftBinary(Op.Dot, (x, y) => alg.dot(x, y), t1, t2)

    override def reduceSum(t: T): F[T] =
      liftUnary(Op.ReduceSum, x => alg.reduceSum(x), t)

    override def variable(name: String)(t: T): F[T] =
      t match {
        case LeafVertex(p @ Payload(Op.Constant, _, _)) =>
          pure(LeafVertex(p.copy(op = Op.Variable, name = name.some)))
        case _  => me.raiseError(new Throwable("only a leaf constant node could be converted to a variable"))
      }

  }

}
