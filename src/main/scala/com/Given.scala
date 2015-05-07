package com

import scala.util.Random

class Given[T](probs: Seq[(Double, T)]) extends Distribution[T] {
	val rnd = new Random
	val aggrProbs = probs.scanLeft(0.0)((p, x) => p + x._1).drop(1).zip(probs.map(_._2))

	def sample: T = {
		val r = rnd.nextDouble()
		aggrProbs.find(x => x._1 >= r).get._2
	}
}