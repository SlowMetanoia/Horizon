package swing

import java.awt.Toolkit
import java.awt.event.ActionEvent
import javax.swing.{JButton, JFrame, UIManager}

object LookAndFeel extends App{
  val lookAndFeels: Array[UIManager.LookAndFeelInfo] = UIManager.getInstalledLookAndFeels
  println(lookAndFeels.mkString("\n"))

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
  jFrame.setTitle("MyApp")
  val butts = for (i<-0 to 2) yield {
    val butt = new JButton(s"butt$i")
    butt.setBounds(0,40*i,100,35)
    butt
  }
  butts(0).addActionListener((e:ActionEvent) => UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel"))
  butts(1).addActionListener((e:ActionEvent) => UIManager.setLookAndFeel("javax.swing.plaf.nimbus.NimbusLookAndFeel"))
  butts(2).addActionListener((e:ActionEvent) => UIManager.setLookAndFeel("com.sun.java.swing.plaf.motif.MotifLookAndFeel"))
  butts.foreach(jFrame.add)
  jFrame.repaint()
}
