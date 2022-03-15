package SADMOR.Simplex

import scala.language.implicitConversions

case class Rational(a: Int, b: Int) extends Ordering[Rational]{
  def NOD: Int => Int => Int = a => b => {
    if (a == 0 || b == 0)
      a max b
    else {
      (a >= 0, b >= 0) match {
        case (false, false) => NOD(-a)(-b)
        case (true, false) => NOD(a)(-b)
        case (false, true) => NOD(-a)(b)
        case (true, true) =>
          val x = a max b
          val y = a min b
          x % y match {
            case 0 => y
            case _ => NOD(x % y)(y)
          }
      }
    }
  }

  def NOK: Int => Int => Int = a => b => a / NOD(a)(b) * b

  def simplify(): Rational = {
    val c = NOD(a)(b)
    if(b>0) Rational(a / c, b / c) else -Rational(a / c, -b / c)
  }

  def +(other: Rational): Rational = {
    other match {
      case Rational(x, y) if y == b => Rational(a + x, b)
      case Rational(x, y) =>
        val c = NOK(b)(y) / y
        val d = NOK(b)(y) / b
        Rational(x * c + a * d, y * c)
    }
  }

  def unary_-(): Rational = Rational(-a, b)

  def -(other: Rational): Rational = this + (-other)

  def *(other: Rational): Rational = Rational(other.a * a, other.b * b)

  def /(other: Rational): Rational = {
    other match {
      case Rational(_, 0) => throw new Exception("DivideByZero")
      case Rational(x, y) => Rational(a * y, b * x)
    }
  }

  override def toString: String = s"$a/$b"

  def compare(x: Rational, y: Rational): Int = (x-y).a
}
object Rational{
  implicit def Int2Rational(i: Int): Rational = Rational(i, 1)
  implicit def Rational2Double(r:Rational):Double = (r.a + 0.0)/r.b
}