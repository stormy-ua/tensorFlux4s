package org.tensorFlux4s.computation.graph

case class Payload[A](op: Op, value: A, name: Option[String] = None)
