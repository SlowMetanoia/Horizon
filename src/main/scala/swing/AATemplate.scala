package swing

import java.awt.Toolkit
import javax.swing.JFrame

object AATemplate {

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
}
