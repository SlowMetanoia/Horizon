package swing


import java.awt.{Color, Graphics, Toolkit}
import java.awt.event.{MouseAdapter, MouseEvent, MouseListener}
import javax.swing.{JComponent, JFrame, JPanel}
import scala.swing.Graphics2D

object MouseListener extends App{
  val jFrame = new JFrame()
  val jPanel = new JPanel()
  jFrame.add(jPanel)
  val toolkit = Toolkit.getDefaultToolkit
  val dimension = toolkit.getScreenSize
  val relativeSize = (0.5,0.5)
  val windowSize = (
    (dimension.width*relativeSize._1).toInt,
    (dimension.height*relativeSize._2).toInt
  )
  jFrame.setTitle("MyApp")
  jFrame.setVisible(true)
  jFrame.setDefaultCloseOperation(1)
  jFrame.setBounds(dimension.width/2 - windowSize._1/2,
    dimension.height/2 - windowSize._2/2,
    windowSize._1,
    windowSize._2)
  jFrame.repaint()

  jPanel.addMouseListener(
    new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit = {
        super.mouseClicked(e)
        jPanel.setBackground(Color.BLUE)
      }
    }
  )

  object MyComponent extends JComponent{
    var xC:Int = 0
    var yC:Int = 0
    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]
      g2d.drawString(s"Coordinates: x = $xC; y = $yC",50,50)
    }
  }
  jFrame.add(MyComponent)
  jFrame.addMouseMotionListener(new MouseAdapter {
    override def mouseMoved(e: MouseEvent): Unit = {
      super.mouseMoved(e)
      MyComponent.xC = e.getX
      MyComponent.yC = e.getY
      MyComponent.repaint()
    }
  })
}

