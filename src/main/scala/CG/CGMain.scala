package CG

import java.awt.Graphics
import java.awt.geom.Line2D
import javax.swing.JComponent
import scala.swing.Graphics2D

object CGMain extends App{
  //ряды
  def series[T](prev:T)(next:T=>T):LazyList[T] = prev#::series(next(prev))(next)
  val jFrame = JFrameBasics.jFrame
  val setupTransformation = 
  
  
  jFrame.add(
    new JComponent {
      override def paintComponent( g: Graphics ): Unit = {
        super.paintComponent(g)
        val g2d = g.asInstanceOf[Graphics2D]
        g2d.draw(new Line2D.Double(0,0,1,0))
      }
    }
  )
  jFrame.revalidate()
}

