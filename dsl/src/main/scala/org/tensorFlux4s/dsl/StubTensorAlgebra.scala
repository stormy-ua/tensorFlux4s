package org.tensorFlux4s.dsl


trait StubTensorAlgebra[F[_]] extends TensorAlgebra[F] {

  def pure(t: T): F[T] = ???

  def cons(values: Seq[Double], shape: Int*): F[T] = ???

  def ones(shape: Int*): F[T] = ???

  def zeroes(shape: Int*): F[T] = ???

  def fill(v: Double, shape: Int*): F[T] = ???

  def set(t: T, index: Int, value: Double): F[T] = ???

  def get(t: T, index: Int): F[Double] = ???

  def sum(t1: T, t2: T): F[T] = ???

  def subtract(t1: T, t2: T): F[T] = ???

  def multiply(t1: T, t2: T): F[T] = ???

  def dot(t1: T, t2: T): F[T] = ???

  def divide(t1: T, t2: T): F[T] = ???

  def greaterThan(t1: T, t2: T): F[T] = ???

  def reduceSum(t: T): F[T] = ???

  def length(t: T): F[Int] = ???

  def shape(t: T): F[Array[Int]] = ???

  def map(t: T)(f: Double => Double): F[T] = ???

  def transpose(t: T): F[T] = ???

  def variable(name: String)(t: T): F[T] = ???

  def maximum(t1: T, t2: T): F[T] = ???

  def exp(t: T): F[T] = ???

}
