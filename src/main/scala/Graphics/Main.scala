package Graphics

import java.awt.{ Color, Toolkit }
import java.awt.geom.Point2D

object Main {
  class Palette( red: Point2D => Int, green:Point2D => Int, blue:Point2D => Int ) {
    def color( p:Point2D ) = new Color(red(p),green(p),blue(p))
  }
  def main( args: Array[ String ] ): Unit = {
    val toolkit = Toolkit.getDefaultToolkit
    val dimension = toolkit.getScreenSize
    val relativeSize = (0.5, 0.5)
  }
}
