package swing.fractal



import java.awt.geom.{ Line2D, Point2D }
import java.awt.{ Color, Graphics, Toolkit }
import javax.swing.{ JComponent, JFrame }
import scala.swing.Graphics2D

object firstFractalApproach extends App {
  val toolkit = Toolkit.getDefaultToolkit
  val dimension = toolkit.getScreenSize
  val relativeSize = (0.5, 0.5)
  
  def stepFunc:Double=>Int = a=> if(a>=2) 100 else 200
  
  case class Pixel(point2D: Point2D,color: Color)
  
  def pixel2Point( point2D: Point2D, unitSize: Int ): (Double, Double) = (
    (point2D.getX) / unitSize,
    (point2D.getY) / unitSize
  )
  
  class Palette( red: Double => Int, green: Double => Int, blue: Double => Int ) {
    def color( x: Double ) = new Color(red(x), green(x), blue(x))
  }
  
  def mandelbrotPixel( point2D: Point2D, palette: Palette, unitSize: Int, iterationNumber:Int): Pixel = {
    val point = pixel2Point(point2D, unitSize)
    val result = mandelbrotAttractor(point)(iterationNumber) match { case (x,y) => scala.math.sqrt(x*x + y*y) }
    Pixel(point2D,palette.color(result))
  }
  
  def recurrentSeries[ T ]( prev: T )( next: T => T ): LazyList[ T ] = prev #:: recurrentSeries(next(prev))(next)
  
  def mandelbrotAttractor(start:(Double,Double)): LazyList[(Double, Double)] =
    recurrentSeries[(Double,Double)](start){ case (x,y) => (x*x - y*y + start._1,2*x*y + start._2) }
  
  val windowSize = (
    ( dimension.width * relativeSize._1 ).toInt,
    ( dimension.height * relativeSize._2 ).toInt
  )
  
  val palette = new Palette(stepFunc,stepFunc,stepFunc)
  println(windowSize._1)
  
  val pixels: Seq[Pixel] = for{
    i<-0 to windowSize._1
    j<-0 to windowSize._2
  } yield mandelbrotPixel(new Point2D.Double(i,j),palette,100,100)
  //println(pixels)
  
  val jFrame = new JFrame()
  
  
  
  
  jFrame.setVisible(true)
  jFrame.setDefaultCloseOperation(1)
  jFrame.setBounds(dimension.width / 2 - windowSize._1 / 2,
                   dimension.height / 2 - windowSize._2 / 2,
                   windowSize._1,
                   windowSize._2)
  
  object Mandelbrot extends JComponent {
    val windowSize: (Int, Int) = (jFrame.getWidth, jFrame.getHeight)
  
    def drawPixel( g2d: Graphics2D, point2D: Point2D, color: Color ): Unit = {
      g2d.setColor(color)
      g2d.draw(new Line2D.Double(point2D, point2D))
    }
    
    override protected def paintComponent( g: Graphics ): Unit = {
      val g2d = g.asInstanceOf[ Graphics2D ]
      pixels.foreach(pixel=>drawPixel(g2d,pixel.point2D,pixel.color))
    }
  }
  
  
  jFrame.add(Mandelbrot)
  
  jFrame.setTitle("MyApp")
  jFrame.repaint()
}