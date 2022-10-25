package swing

import CG.JFrameBasics

import javax.swing.{ JPanel, JTextField }

object Panels extends App{
  val jFrame = JFrameBasics.jFrame
  val jPanel = new JPanel()
  val textField = new JTextField()
  textField.setText("some text")
  jPanel.add(textField)
  jFrame.add(jPanel)
  jFrame.revalidate()
}
