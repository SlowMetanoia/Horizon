package swing

import java.awt.{Color, Toolkit}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{AbstractAction, Action, JButton, JFrame, JPanel}

object Actions extends App{

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

  val jPanel = new JPanel()
  val jButton = new JButton(MyAction)
  jButton.setText("submit")
  //jButton.setBounds(10,10,70,20)

  jFrame.add(jPanel)
  jPanel.add(jButton)

  jFrame.setTitle("MyApp")

  object MyAction extends AbstractAction{
    override def actionPerformed(e: ActionEvent): Unit = {
      jPanel.setBackground(new Color(20,100,150))
    }
    putValue(Action.SHORT_DESCRIPTION,"does some little action")
  }
  jFrame.repaint()
}
