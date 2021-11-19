package Shorts.Simplex

import scala.language.implicitConversions

object Main extends App {
  val basis = Array("x1", "x2", "x3")
  val free = Array("y1", "y2", "y3")


  val b = Array(Array(2, 3), Array(4, 5), Array(8, 7))
  val c = b.map(_.map(Rational(_, 1)))
  var smpx2 = SimplexMethodCalculations()
  println(smpx2)
  smpx2 = smpx2.simplexTransformation(1, 3)
  println(smpx2)
  smpx2 = smpx2.simplexTransformation(1,3)
  println(smpx2)
}