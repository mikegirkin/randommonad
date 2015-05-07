package com

object main {
  def intHisto(dist: Distribution[Int]) = {
    dist.histo
      .sortWith((t1, t2) => t1._1 > t2._1)
      .map {
        case (v: Int, f: Double) => f"$v: $f%5.4f"
      }
      .mkString(sys.props("line.separator"))
  }

  def strHisto(dist: Distribution[String]) = {
    dist.histo
      .map {
        case (v: String, f: Double) => f"$v: $f%5.4f"
      }
      .mkString(sys.props("line.separator"))
  }

  def dice(sides: Int) = Uniform(Range(1, sides+1, 1))

  def aggressive = new Given(Seq(
    (0.9 -> true), 
    (0.1 -> false)
  ))

  def normal = new Given(Seq(
    (0.2 -> true), 
    (0.8 -> false)
  ))

  def cautious = new Given(Seq(
    (0.1 -> true), 
    (0.9 -> false)
  ))

  def driver = new Given(Seq(
    (0.2 -> cautious),
    (0.6 -> normal),
    (0.2 -> aggressive)
  ))

  def main(args: Array[String]) = {
    val d6 = dice(6)
    val d3d6 = for {
      x <- d6
      y <- d6
      z <- d6
    } yield x+y+z

    val game = for {
      hit <- dice(20)
      damage <- dice(6)
      saveThrow <- dice(10)
      spellSuccess <- dice(100)
    } yield {
      if(hit < 10) "Eaten by monster"
      else if (damage < 4) "Eaten by monster"
      else if (spellSuccess < 43) "Couldn't open the door"
      else if (saveThrow < 5) "Killed by the trap"
      else s"Success!"
    }

    val family = new Uniform(Array("Boy", "Girl")).until(children => children.head == "Boy")

    val population = for {
      children <- family.repeat(10).map(_.flatten)
      girls = children.count(_ == "Girl")
    } yield girls

    val collision = for {
        d1 <- driver
        d2 <- driver
        act1 <- d1
        act2 <- d2
    } yield (act1 && act2).toString()



    println("d6 roll --------------------")
    println(s"${intHisto(d6)}")
    println("----------------------------")
    println("3d6 roll -------------------")
    println(s"${intHisto(d3d6)}")
    println("----------------------------")
    println("Adventure ------------------")
    println(s"${strHisto(game)}")
    println("----------------------------")
    println("Population -----------------")
    println(s"${intHisto(population)}")
    println("----------------------------")
    println("Collision on Yellow --------")
    println(s"${strHisto(collision)}")
    println("----------------------------")    
  }
}
