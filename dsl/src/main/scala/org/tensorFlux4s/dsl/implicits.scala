package org.tensorFlux4s.dsl

import cats.Monad

object implicits {

  implicit class Extensions[F[_], T](val self: T) extends AnyVal {

    def +(other: T)(implicit alg: Aux[F, T]): F[T] = alg.sum(self, other)

    def -(other: T)(implicit alg: Aux[F, T]): F[T] = alg.subtract(self, other)

    def *(other: T)(implicit alg: Aux[F, T]): F[T] = alg.multiply(self, other)

    def /(other: T)(implicit alg: Aux[F, T]): F[T] = alg.divide(self, other)

    def dot(other: T)(implicit alg: Aux[F, T]): F[T] = alg.dot(self, other)

    def maximum(other: T)(implicit alg: Aux[F, T]): F[T] = alg.maximum(self, other)

    def T(implicit alg: Aux[F, T]): F[T] = alg.transpose(self)

    def reduceSum(implicit alg: Aux[F, T]): F[T] = alg.reduceSum(self)

    def \+/(implicit alg: Aux[F, T]): F[T] = reduceSum

    def length(implicit alg: Aux[F, T]): F[Int] = alg.length(self)

    def shape(t: T)(implicit alg: Aux[F, T]): F[Array[Int]] = alg.shape(self)

    def map(f: Double => Double)(implicit alg: Aux[F, T]): F[T] = alg.map(self)(f)

    def asVariable(name: String)(implicit alg: Aux[F, T]): F[T] = alg.variable(name)(self)

  }

  implicit class ExtensionsF[F[_], T](val self: F[T]) extends AnyVal {

    def +(other: F[T])(implicit alg: Aux[F, T], m: Monad[F]): F[T] = alg.sum(self, other)

    def -(other: F[T])(implicit alg: Aux[F, T], m: Monad[F]): F[T] = alg.subtract(self, other)

    def *(other: F[T])(implicit alg: Aux[F, T], m: Monad[F]): F[T] = alg.multiply(self, other)

    def /(other: F[T])(implicit alg: Aux[F, T], m: Monad[F]): F[T] = alg.divide(self, other)

    def dot(other: F[T])(implicit alg: Aux[F, T], m: Monad[F]): F[T] = alg.dot(self, other)

    def maximum(other: F[T])(implicit alg: Aux[F, T], m: Monad[F]): F[T] = alg.maximum(self, other)

    def reduceSum(implicit alg: Aux[F, T], m: Monad[F]): F[T] = alg.reduceSum(self)

    def \+/(implicit alg: Aux[F, T], m: Monad[F]): F[T] = reduceSum

  }

}
