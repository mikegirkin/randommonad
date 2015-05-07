package com

import scala.util.Random

class Uniform[T](values: Array[T]) extends Distribution[T] {
  val rnd = new Random

  def sample: T = {
    val index = rnd.nextInt(values.size)
    values(index)
  }
}

object Uniform {
  def apply(range: Range): Distribution[Int] = {
    new Uniform(range.toArray)
  }
}


