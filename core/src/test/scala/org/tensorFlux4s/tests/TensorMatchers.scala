package org.tensorFlux4s
package tests

import org.nd4j.linalg.api.ndarray.INDArray
import org.scalatest._
import org.nd4j.linalg.factory.Nd4j
import org.nd4s.Implicits._
import org.nd4j.linalg.api.ops.impl.transforms.comparison._


trait TensorMatchers extends Matchers { self: FlatSpec =>

  def assertAreEqual(v1: INDArray, v2: INDArray, precision: Double = 1E2): Unit = {
    v1.shape shouldBe v2.shape

    for {
      col <- 0 until v1.columns
      row <- 0 until v1.rows
    } yield v1.getColumn(col)(row) should be (v2.getColumn(col)(row) +- precision)
  }

}

