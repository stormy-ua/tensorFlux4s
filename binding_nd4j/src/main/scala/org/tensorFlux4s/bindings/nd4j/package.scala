package org.tensorFlux4s
package bindings

import dsl._

package object nd4j {

  import org.nd4j.linalg.api.ndarray.INDArray
  import org.nd4j.linalg.factory.Nd4j
  import org.nd4s.Implicits._
  import org.nd4j.linalg.ops.transforms.Transforms

  type F[A] = Either[Throwable, A]

  implicit val algebra = new TensorAlgebra[F] {

    type T = INDArray

    implicit val aux: Aux[F, T] = this

    private def catchNonFatal[A](f: => A): Either[Throwable, A] =
      try {
        Right(f)
      } catch {
        case scala.util.control.NonFatal(t) => Left(t)
      }

    def pure(t: T) = catchNonFatal(t)

    def cons(values: scala.Seq[Double], shape: Int*) =
      catchNonFatal[INDArray](values.asNDArray(shape: _*))

    def sum(t1: T, t2: T) = catchNonFatal(t1 + t2)

    def ones(shape: Int*) = catchNonFatal(Nd4j.ones(shape:_*))

    def zeroes(shape: Int*) = catchNonFatal(Nd4j.zeros(shape:_*))

    def exp(t: T) = catchNonFatal(Transforms.exp(t))

    def set(t: T, index: Int, value: Double) = catchNonFatal(t.dup(index) = value)

    def get(t: T, index: Int) = catchNonFatal(t(index))

    def fill(v: Double, shape: Int*) = catchNonFatal {
      Nd4j.create(Array.fill(shape.product)(v), shape.toArray)
    }

    def subtract(t1: T, t2: T) = catchNonFatal(t1 - t2)

    def multiply(t1: T, t2: T) = catchNonFatal(t1 * t2)

    def dot(t1: T, t2: T) = catchNonFatal(t1 dot t2)

    def divide(t1: T, t2: T) = catchNonFatal(t1 / t2)

    def greaterThan(self: T, other: T) = catchNonFatal {
      (0 until self.length).map { i =>
        val a = self.data.asDouble()(i)
        val b = other.data.asDouble()(i)

        if (a > b) 1.0 else 0.0
      }.asNDArray(self.shape: _*)
    }

    def maximum(self: T, other: T) = catchNonFatal {
      (0 until self.length).map { i =>
        val a = self.data.asDouble()(i)
        val b = other.data.asDouble()(i)

        math.max(a, b)
      }.asNDArray(self.shape: _*)
    }

    def reduceSum(t: T) = catchNonFatal(Seq(t.sumNumber.doubleValue).asNDArray(1, 1))

    def length(t: T) = catchNonFatal(t.length)

    def shape(t: T) = catchNonFatal(t.shape)

    def map(t: T)(f: Double => Double) = catchNonFatal(t.map(f))

    def transpose(t: T) = catchNonFatal(t.T)

    def variable(name: String)(t: T) = catchNonFatal(t)
  }

}
