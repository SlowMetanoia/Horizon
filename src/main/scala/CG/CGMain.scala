package CG

import java.awt.{Color, Toolkit}
import javax.swing.{JFrame, JPanel}

object CGMain extends App{
  //-----------------------------------Template--------------------------------------------------
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
  //--------------------------------------------------New code---------------------------------------

}
