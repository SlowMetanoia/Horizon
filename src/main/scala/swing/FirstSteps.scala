package swing
//Вот это вот недостойно. Давайте ещё одни библиотеку притянем просто, чтобы узнать ширину и высоту экрана. Зашибись.
import java.awt.geom.{ Ellipse2D, Line2D, Point2D }
import java.awt.{ Graphics, Toolkit }
import java.net.URL
import javax.swing.{ ImageIcon, JComponent, JFrame }
import scala.swing.{ Color, Graphics2D, Image }

object FirstSteps extends App{
  val jFrame = new JFrame()
  val toolkit = Toolkit.getDefaultToolkit
  val dimension = toolkit.getScreenSize
  val relativeSize = (0.5,0.5)
  val windowSize = (
    (dimension.width*relativeSize._1).toInt,
    (dimension.height*relativeSize._2).toInt
  )
  jFrame.setVisible(true)
  jFrame.setDefaultCloseOperation(1)
  jFrame.setBounds(dimension.width/2 - windowSize._1/2,
                   dimension.height/2 - windowSize._2/2,
                   windowSize._1,
                   windowSize._2)
  jFrame.setTitle("MyApp")
  jFrame.add(new MyComponent)
  jFrame.repaint()
  class MyComponent extends JComponent(){
    //val myFont: Font = Font("Arial", Font.Plain, 20)
    //val fonts = GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames()
    //println(fonts.mkString("\n"))
    override protected def paintComponent(g:Graphics): Unit = {
      val g2d = g.asInstanceOf[Graphics2D]
      //g2d.setFont(myFont)
      //g2d.drawString("wellow hold!",20,20)
      
      val p1 = new Point2D.Double(50,50)
      val p2 = new Point2D.Double(150,50)
      val l2d = new Line2D.Double(p1,p2)
      g2d.draw(l2d)
      
      g2d.setPaint(new Color(255,100,200))
      val el = new Ellipse2D.Double(50,50,100,150)
      g2d.draw(el)
      g2d.fill(el)
      
      val url = new URL("https://static.javatpoint.com/images/swinghierarchy.jpg")
      val image:Image = new ImageIcon(url).getImage
      
      g2d.drawImage(image,220,70,null)
      //revalidate()
      
      
    }
    
  }
  jFrame.getGraphics.drawLine(0,0,40,40)
}

