package CG

import java.awt.geom.AffineTransform
import java.awt.{ BasicStroke, Stroke, Toolkit }
import javax.swing.JFrame
import scala.swing.Dimension

object JFrameBasics {
  val jFrame: JFrame = new JFrame()
 
  val dimension: Dimension = Toolkit.getDefaultToolkit.getScreenSize
  
  val relativeScreenSize: (Double, Double)= (0.5, 0.5)
  

  val windowSize = (
    (dimension.width*relativeScreenSize._1).toInt,
    (dimension.height*relativeScreenSize._2).toInt
  )
  
  val windowCenter: (Int, Int) = (windowSize._1 /2, windowSize._2 /2)
  
  //Из центра экрана
  val startTransposition = new AffineTransform(1,0,0,-1,windowCenter._1,windowCenter._2)
  
  //Относительный размер единицы
  val unitRelativeSize = 40
  
  startTransposition.concatenate(new AffineTransform(unitRelativeSize,0,0,unitRelativeSize,0,0))
  
  val stroke: Stroke = new BasicStroke(0)
  
  jFrame.setTitle("MyApp")
  jFrame.setVisible(true)
  jFrame.setDefaultCloseOperation(1)
  jFrame.setBounds(dimension.width/2 - windowSize._1/2,
                   dimension.height/2 - windowSize._2/2,
                   windowSize._1,
                   windowSize._2)
}

