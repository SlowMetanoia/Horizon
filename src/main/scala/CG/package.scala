import java.awt.Color
import java.awt.geom.Point2D

package object CG {
  //ряды
  def series[T](prev: T)(next: T => T): LazyList[T] = prev #:: series(next(prev))(next)
  case class Pixel(point2D: Point2D,color: Color)
  class Palette( red: Double => Int, green: Double => Int, blue: Double => Int ) {
    def color( x: Double ) = new Color(red(x), green(x), blue(x))
  }

}
