package CG

import java.awt.{ Color, GridBagConstraints, GridBagLayout }
import javax.swing.event.ChangeEvent
import javax.swing.{ JPanel, JSlider, JTextArea }

object FormsMain extends App {
  val jFrame = JFrameBasics.jFrame
  class ColorChoosePanel extends JPanel{
    setLayout(new GridBagLayout)
    val (r,g,b) = (
      new JSlider(0,255,0),
      new JSlider(0,255,0),
      new JSlider(0,255,0)
    )
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
    colorConstraint.weightx = 0
    colorConstraint.weighty = 0
    colorConstraint.gridx = 3
    colorConstraint.gridy = 0
    colorConstraint.gridheight = 3
    colorConstraint.gridwidth = 1

    val colorField = new JTextArea(4,4)
    colorField.setBackground(color)

    add(r,rsConstraint)
    add(g,gsConstraint)
    add(b,bsConstraint)
    add(colorField,colorConstraint)
    revalidate()
  
    println(s"bsConstraint.anchor = ${ bsConstraint.anchor }")
    println(s"gsConstraint.anchor = ${ gsConstraint.anchor }")
    println(s"rsConstraint.anchor = ${ rsConstraint.anchor }")
    println(s"bsConstraint.fill = ${ bsConstraint.fill }")
    println(s"bsConstraint.insets = ${ bsConstraint.insets }")
    println(s"bsConstraint.ipadx = ${ bsConstraint.ipadx }")
    println(s"bsConstraint.ipady = ${ bsConstraint.ipady }")
  }
  jFrame.add(new ColorChoosePanel)
  jFrame.revalidate()
}
