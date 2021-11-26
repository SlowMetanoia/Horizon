package Shorts.Computational

object Main extends App{
  println(polynomialDerivative(IndexedSeq(1250.0,0,2,7),7.0))
  def polynomial(x:Double): Double = 1250 + 2*x*x + 7*x*x*x
  println(derivative(polynomial,7))
}
