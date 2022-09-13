package swing

import java.awt.{ Color, Toolkit }
import java.awt.event.ActionEvent
import javax.swing.{ JButton, JFrame, JPanel }

object ActionListener extends App{
  val jFrame = new JFrame()
  val jPanel = new JPanel()
  //Ну, во всяком случае, это более красиво, чем создавать отдельный, пусть анонимный класс ради одного метода.
  val jButton = new JButton("Submit")
  jButton.addActionListener((e:ActionEvent)=>jPanel.setBackground(Color.cyan))
  jPanel.add(jButton)
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
  jFrame.add(jPanel)
}
