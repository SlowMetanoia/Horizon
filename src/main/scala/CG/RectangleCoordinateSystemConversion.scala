package CG

import java.awt.{Color, Graphics}
import java.awt.geom.Point2D
import scala.swing.Graphics2D

class RectangleCoordinateSystemConversion(
                                           xConversion:Double,
                                           yConversion:Double,
                                           shift:Vector2D = Vector2D(0,0)
                                         ) {
  /**
   * Координаты в прямоугольной системе координат
   * @param x первая координата точки в экранной системе координат
   * @param y вторая координата точки в экранной системе координат
   * @return вектор в этой системе координат
   */
  def fromScreenCoordinates(x:Double,y:Double):Vector2D = {
    Vector2D(
      x / xConversion,
      y / yConversion
    ) - shift
  }
  def toScreenCoordinates(v2D:Vector2D):(Double,Double) = {
    val shifted = v2D+shift
    (
      shifted.x*xConversion,
      shifted.y*yConversion
    )
  }
}
object RectangleCoordinateSystemConversion extends App {
  val rcs = new RectangleCoordinateSystemConversion(-10,10, Vector2D(1,3))
  val (x,y) = (30,50)
  val vPoint = rcs.fromScreenCoordinates(x,y)
  println(s"vPoint = $vPoint")
  println(s"rcs.toScreenCoordinates(vPoint) = ${rcs.toScreenCoordinates(vPoint)}")
}
