package org.tensorFlux4s

package object dsl {

  type Aux[F[_], A] = TensorAlgebra[F] { type T = A }

}
