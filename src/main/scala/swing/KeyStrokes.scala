package swing

import swing.Actions.MyAction.putValue

import java.awt.event.ActionEvent
import java.awt.{Color, Toolkit}
import javax.swing.{AbstractAction, Action, InputMap, JButton, JComponent, JFrame, JPanel, KeyStroke}

object KeyStrokes extends App {

  final object ks


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
  object MyAction extends AbstractAction{
    override def actionPerformed(e: ActionEvent): Unit = {
      jPanel.setBackground(new Color(20,100,150))
    }
    putValue(Action.SHORT_DESCRIPTION,"does some little action")
  }
  val jButton = new JButton(MyAction)
  val keyStroke = KeyStroke.getKeyStroke("ctrl B")

  val inputMap = jPanel.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
  inputMap.put(keyStroke,ks)
  val actionMap = jPanel.getActionMap
  actionMap.put(ks,MyAction)

  jButton.setText("submit")
  //jButton.setBounds(10,10,70,20)

  jFrame.add(jPanel)
  jPanel.add(jButton)

  jFrame.setTitle("MyApp")

  jFrame.repaint()
}
