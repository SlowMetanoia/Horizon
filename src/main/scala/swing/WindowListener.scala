package swing

import java.awt.Toolkit
import java.awt.event.{WindowAdapter, WindowEvent, WindowListener}
import javax.swing.JFrame

object WindowListener extends App{

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
  /*
  * WindowListener нужно переопределять полностью
  * WindowAdapter - как WindowListener, но все методы заданы и пусты
  */
  jFrame.addWindowListener(new WindowAdapter(){
    override def windowClosing(e: WindowEvent): Unit = {
      super.windowClosing(e)
      println("goodbye world!")
    }
  })
}
