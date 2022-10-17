package CG

import java.awt.{Rectangle, Shape}
import java.awt.geom.{AffineTransform, PathIterator, Point2D, Rectangle2D}

abstract class ShapeMapper extends Shape{
  var mappedShape:Shape

  override def getBounds: Rectangle = mappedShape.getBounds

  override def getBounds2D: Rectangle2D = mappedShape.getBounds2D

  override def contains(x: Double, y: Double): Boolean = mappedShape.contains(x,y)

  override def contains(p: Point2D): Boolean = mappedShape.contains(p)

  override def intersects(x: Double, y: Double, w: Double, h: Double): Boolean = mappedShape.intersects(x,y,w,h)

  override def intersects(r: Rectangle2D): Boolean = mappedShape.intersects(r)

  override def contains(x: Double, y: Double, w: Double, h: Double): Boolean = mappedShape.contains(x,y,w,h)

  override def contains(r: Rectangle2D): Boolean = mappedShape.contains(r)

  override def getPathIterator(at: AffineTransform): PathIterator = mappedShape.getPathIterator(at)

  override def getPathIterator(at: AffineTransform, flatness: Double): PathIterator = mappedShape.getPathIterator(at,flatness)
}