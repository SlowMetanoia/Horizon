package CG

import javax.swing.{JLabel, JPanel, JTextField}

object ProcTextures extends App{
  val jFrame = JFrameBasics.jFrame
  class displayableAffine(m00: Double, m01: Double, m02: Double,
                          m10: Double, m11: Double, m12: Double) extends JPanel {

    val tfm00 = new JTextField(s"$m00")
    val tfm01 = new JTextField(s"$m01")
    val tfm02 = new JTextField(s"$m02")
    val tfm10 = new JTextField(s"$m10")
    val tfm11 = new JTextField(s"$m11")
    val tfm12 = new JTextField(s"$m12")

    add(new JLabel("x coefficient"))
    add(tfm00)
    add(tfm01)
    add(tfm02)
    add(tfm10)
    add(tfm11)
    add(tfm12)

  }
  jFrame.add( new displayableAffine(0,1,2,3,4,5))
  jFrame.revalidate()
}
