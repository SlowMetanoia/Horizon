package Shorts

import scala.math.{abs, pow}

package object Computational {
  /**
   * численно вычисленная производная в точке
   * @param f
   * @param x
   * @param eps
   * @return
   */
  def derivative(f: Double=>Double,x:Double,eps:Double = 0.01,iterations:Int = 100):(Double,Double) = {
    def derivative(delta:Double):Double = (f(x+delta) - f(x-delta))/(2*delta)
    def derivativeSequence(delta:Double):LazyList[Double] =
      derivative(delta)#::derivativeSequence(
        if(eps>1)delta/eps else delta*eps
      )
    var valueList = derivativeSequence(delta = eps)
    var oldEps = Double.MaxValue
    var oldVal = valueList.head
    valueList = valueList.tail
    for (i<- 1 until iterations){
      if(oldEps<eps) return (oldVal,oldEps)
      oldEps = abs(oldVal-valueList.head)
      oldVal = valueList.head
      valueList = valueList.tail
    }
    (oldVal,oldEps)
  }

  /**
   * @param coefficients коэффициенты перед x**i
   * @param x точка расчёта
   * @return производную полинома в точке x
   */
  def polynomialDerivative(coefficients:IndexedSeq[Double],x:Double):Double =
    coefficients.indices.map{
      i=>coefficients(i)*i*pow(x,i-1)
    }.tail.sum
  //(x-x1)(x-x2)(x-x3)...() = x^n - sum(xi)*x^(n-1) + () -
  def makePolynomial(roots: IndexedSeq[Double]):IndexedSeq[Double] =
    roots.indices.appended(roots.length)
      .map{
      i=>
        math.pow(-1,i)*
          roots.combinations(i)
            .map(_.product).sum
    }
}
