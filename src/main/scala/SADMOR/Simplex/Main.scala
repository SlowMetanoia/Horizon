package SADMOR.Simplex

import scala.language.implicitConversions

object Main extends App {
  val basis = Array("x1", "x2", "x3")
  val free = Array("y1", "y2", "0")

  def step( table: SimplexTable )(x:Int,y:Int) = {
    println(table.simplexTransformation(x,y))
    println()
    table.simplexTransformation(x,y)
  }

  val b = Array(Array(3,0,4, 36), Array(3,0,2,24), Array(1,1,0,6), Array(-7,-1,-3,0))
  val c = b.map(_.map(Rational(_, 1)))
  var table = SimplexTable(c, free, basis)
  table = step(table)(3,1)
  table = step(table)(2,3)
}