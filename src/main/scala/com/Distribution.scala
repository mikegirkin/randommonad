package com
import java.util.concurrent.ThreadLocalRandom
import scala.util.Random

trait Distribution[T] {
  self =>

  def sample(): T

  def flatMap[TOut](f: T => Distribution[TOut]): Distribution[TOut] = {
    new Distribution[TOut] {
      override def sample = f(self.sample).sample
    }
  }

  def map[TOut](f: T => TOut): Distribution[TOut] = {
    new Distribution[TOut] {
      override def sample() = f(self.sample())
    }
  }

  def until(pred: List[T] => Boolean): Distribution[Seq[T]] = {
    new Distribution[Seq[T]] {
      override def sample: Seq[T] = {
        def iter(acc: List[T]): List[T] = {
          val s = self.sample :: acc
          if(pred(s)) s
          else iter(s)
        }
        iter(Nil)
      }
    }
  }

  def repeat(count: Int): Distribution[Seq[T]] = {
    new Distribution[Seq[T]] {
      override def sample: Seq[T] = {
        List.fill(count)(self.sample())
      }
    }
  }

  def histo: Seq[(T, Double)] = {
    val n = 1000000
    val map = scala.collection.mutable.Map[T, Double]()
    for(i <- 1 to n) {
      val s = sample()
      if(map.isDefinedAt(s)) map(s) = map(s) + 1.0/n
      else map(s) = 1.0/n
    }
    map.toSeq
  }
}

object Distribution {
  private val rnd = Random

  def uniform[T](values: Array[T]): Distribution[T] = new Distribution[T] {
    override def sample: T = {
      val index = rnd.nextInt(values.size)
      values(index)
    }
  }  

  def given[T](probs: Seq[(Double, T)]): Distribution[T] = new Distribution[T] {
    val aggrProbs = probs.scanLeft(0.0)((p, x) => p + x._1).drop(1).zip(probs.map(_._2))
    override def sample: T = {
      val r = rnd.nextDouble()
      aggrProbs.find(x => x._1 >= r).get._2
    }  
  }
}
