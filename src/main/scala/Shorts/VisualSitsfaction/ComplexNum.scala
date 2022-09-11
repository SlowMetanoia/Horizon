package Shorts.VisualSitsfaction

case class ComplexNum(Re:Double,Im:Double) extends Numeric[ComplexNum]{
  def + (that: ComplexNum): ComplexNum = ComplexNum(Re+that.Re,Im+that.Im)
  def unary_- : ComplexNum = ComplexNum(-Re,-Im)
  def * (that:ComplexNum): ComplexNum = ComplexNum( Re*that.Re-Im*that.Im , Re*that.Im + Im*that.Re )
  def - (that:ComplexNum): ComplexNum = this + (- that)
  def / (that:ComplexNum) = ???

  override def plus(x: ComplexNum, y: ComplexNum): ComplexNum = ComplexNum(x.Re+y.Re,x.Im+y.Im)

  override def minus(x: ComplexNum, y: ComplexNum): ComplexNum = ComplexNum(x.Re-y.Re,x.Im-y.Im)

  override def times(x: ComplexNum, y: ComplexNum): ComplexNum = ComplexNum( x.Re*y.Re-x.Im*y.Im , x.Re*y.Im + x.Im*y.Re )

  override def negate(x: ComplexNum): ComplexNum = ComplexNum(-Re,-Im)

  override def fromInt(x: Int): ComplexNum = ComplexNum(x,0)

  override def parseString(str: String): Option[ComplexNum] = ???

  override def toInt(x: ComplexNum): Int = ???

  override def toLong(x: ComplexNum): Long = ???

  override def toFloat(x: ComplexNum): Float = ???

  override def toDouble(x: ComplexNum): Double = ???

  override def compare(x: ComplexNum, y: ComplexNum): Int = ???
}

object ComplexNum{
  implicit def double2ComplexNum:Double=>ComplexNum = ComplexNum(_,0)
  implicit def doubleTuple2ComplexNum: ((Double, Double)) => ComplexNum = (that:(Double,Double)) => ComplexNum(that._1,that._2)
  implicit def int2ComplexNum:Int=>ComplexNum = ComplexNum(_,0)
}