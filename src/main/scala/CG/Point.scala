package CG

import CG.Point._

import java.awt.Shape
import java.awt.event.{MouseAdapter, MouseEvent, MouseListener, MouseMotionListener}
import java.awt.geom.Point2D

class Point(
             var x:Double,
             var y:Double,
             rcs:RectangleCoordinateSystemConversion,
           ){
  def point2D: Point2D = {rcs.toScreenCoordinates(Vector2D(x,y)) match {
    case (x,y) => new Point2D.Double(x,y)
  }}
}
object Point{

}