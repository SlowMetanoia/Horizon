package CG

import java.awt.{Color, Dimension, Graphics, GridBagConstraints, GridBagLayout}
import javax.swing.event.ChangeEvent
import javax.swing.{JComponent, JPanel, JSlider, JTextArea}
import scala.swing.Graphics2D

object FormsMain extends App {
  val jFrame = JFrameBasics.jFrame
  class ColorChoosePanel extends JPanel{
    setLayout(new GridBagLayout)
    val (r,g,b) = (
      new JSlider(0,255,0),
      new JSlider(0,255,0),
      new JSlider(0,255,0)
    )
    r.setMinorTickSpacing(1)
    var color = new Color(r.getValue,g.getValue,b.getValue)
    def recolor(e:ChangeEvent) = {
      color = new Color(r.getValue,g.getValue,b.getValue)
      colorField.setBackground(color)
    }
    r.addChangeListener(recolor)
    g.addChangeListener(recolor)
    b.addChangeListener(recolor)
    val rsConstraint = new GridBagConstraints()
    rsConstraint.weightx = 0
    rsConstraint.weighty = 0
    rsConstraint.gridx = 0
    rsConstraint.gridy = 0
    rsConstraint.gridheight = 1
    rsConstraint.gridwidth = 3

    val gsConstraint = new GridBagConstraints()
    gsConstraint.weightx = 0
    gsConstraint.weighty = 0
    gsConstraint.gridx = 0
    gsConstraint.gridy = 1
    gsConstraint.gridheight = 1
    gsConstraint.gridwidth = 3

    val bsConstraint = new GridBagConstraints()
    bsConstraint.weightx = 0
    bsConstraint.weighty = 0
    bsConstraint.gridx = 0
    bsConstraint.gridy = 2
    bsConstraint.gridheight = 1
    bsConstraint.gridwidth = 3

    val colorConstraint = new GridBagConstraints()
    rsConstraint.weightx = 0
    rsConstraint.weighty = 0
    rsConstraint.gridx = 3
    rsConstraint.gridy = 3
    rsConstraint.gridheight = 3
    rsConstraint.gridwidth = 1

    val colorField = new JTextArea(4,4)
    colorField.setBackground(color)

    add(r,rsConstraint)
    add(g,gsConstraint)
    add(b,bsConstraint)
    add(colorField,colorConstraint)
    revalidate()
  }
  jFrame.add(new ColorChoosePanel)
  jFrame.revalidate()
}
