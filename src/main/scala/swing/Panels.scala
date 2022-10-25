package swing

import CG.JFrameBasics

import java.awt.{Color, Font}
import javax.swing.{JButton, JPanel, JTextField}

object Panels extends App{
  val jFrame = JFrameBasics.jFrame
  val jPanel0 = new JPanel()
  val jPanel1 = new JPanel()
  jPanel0.setBackground(Color.black)
  val textField = new JTextField()
  textField.setBackground(new Color(255,255,255,100))
  textField.setText("some text")
  val submitButton = new JButton("submit")

  //сборка
  jPanel0.add(textField)
  jPanel0.add(submitButton)
  jFrame.add(jPanel0)
  //jFrame.add(jPanel1)
  jFrame.revalidate()
}
