package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
   Signal( b()*b() - 4* a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(
      if(delta() < 0) Set()
      else if(delta() == 0){
        val root = -b()/(2* a())
        Set(root,root)
      }
      else {
        val root1 = (-b() + Math.sqrt(delta()))/(2*a())
        val root2 = (-b() - Math.sqrt(delta()))/(2*a())
        Set(root1,root2)
      }
    )
  }
}
