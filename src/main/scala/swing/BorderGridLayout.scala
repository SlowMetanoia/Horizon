package swing

import CG.JFrameBasics

import java.awt.BorderLayout
import javax.swing.{JButton, JPanel}

object BorderGridLayout extends App{
  val jFrame = JFrameBasics.jFrame
  jFrame.add(new JButton("north"), BorderLayout.NORTH)
  jFrame.add(new JButton("east"), BorderLayout.EAST)
  jFrame.add(new JButton("west"), BorderLayout.WEST)
  jFrame.add(new JButton("south"), BorderLayout.SOUTH)
  jFrame.add(new JButton("center"), BorderLayout.CENTER)
  jFrame.revalidate()
}
