package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {

    Signal{
     Math.pow(b(),2) - 4*a()*c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Signal {

      if (delta() < 0) Set[Double]()
      else {
      val deltaSqrt = Math.sqrt(delta())
      val denom = 2 * a()

      Set((-b() + deltaSqrt) / denom, (-b() - deltaSqrt) / denom)
    }
    }
  }
}
