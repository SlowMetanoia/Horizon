package CG

import CG.CGMain.sifVisual.SIF

import java.awt.geom.{ AffineTransform, Line2D, Point2D }
import java.awt.{ Graphics, Shape, Toolkit }
import javax.swing.{ JComponent, JFrame, JPanel }
import scala.swing.Graphics2D

object CGMain extends App{
  //ряды
  def series[T](prev:T)(next:T=>T):LazyList[T] = prev#::series(next(prev))(next)
  //-----------------------------------Template--------------------------------------------------
  val jFrame = new JFrame()
  val jPanel = new JPanel()
  jFrame.add(jPanel)
  val toolkit = Toolkit.getDefaultToolkit
  val dimension = toolkit.getScreenSize
  
  val relativeSize = (0.5,0.5)
  
  val windowSize = (
    (dimension.width*relativeSize._1).toInt,
    (dimension.height*relativeSize._2).toInt
  )
  
  val windowCenter = (windowSize._1/2,windowSize._2/2)
  println(s"windowCenter = ${ windowCenter }")
  
  //Из центра экрана
  val startTransposition = new AffineTransform(1,0,0,-1,windowCenter._1,windowCenter._2)
  
  //Относительный размер единицы
  //val unitRelativeSize = 40
  
  //startTransposition.concatenate(new AffineTransform(unitRelativeSize,0,0,unitRelativeSize,0,0))
  
  
  jFrame.setTitle("MyApp")
  jFrame.setVisible(true)
  jFrame.setDefaultCloseOperation(1)
  jFrame.setBounds(dimension.width/2 - windowSize._1/2,
    dimension.height/2 - windowSize._2/2,
    windowSize._1,
    windowSize._2)
  jFrame.repaint()
  //--------------------------------------------------New code---------------------------------------
  val p1 = new Point2D.Double(10,10)
  val p2 = new Point2D.Double(100,10)
  val p3 = new Point2D.Double(10,100)
  val cut1 = new Line2D.Double(p1,p2)
  val cut2 = new Line2D.Double(p1,p3)
  val cut3 = new Line2D.Double(p2,p3)

  val xT = new AffineTransform(-0.5,0,0,-0.5,100,100)
  
  
    
  case class sifVisual( initials:Seq[Shape], ats:Seq[AffineTransform]){
    def composeJComponent(lvl:Int):JComponent = {
      new JComponent {
        override def paintComponent( g: Graphics ): Unit = {
          super.paintComponent(g)
          val g2d = g.asInstanceOf[Graphics2D]
          g2d.setTransform(startTransposition)
          SIF(ats)
            .getValuesByLevel(lvl)
            .foreach { xt =>
              g2d.transform(xt)
              initials.foreach(g2d.draw)
              g2d.setTransform(startTransposition)
            }
        }
      }
    }
  }
  
  object sifVisual{
    case class LazyNode[T](value:T,generateChildren:T=>Seq[T],lvl:Int){
      lazy val children: Seq[LazyNode[T]] =
        generateChildren(value)
          .map(value=>
                 LazyNode[T](value,generateChildren,lvl + 1)
               )
    }
    
    case class LazyTree[T](rootValue:T)(generateChildren:T=>Seq[T]){
      val actualTree: LazyNode[T] = LazyNode(rootValue,generateChildren,0)
      def getValuesByLevel:Int=>Seq[T] = {
        case x:Int if x < 0=> Seq.empty[T]
        case 0 => Seq(rootValue)
        case lvl:Int =>
          var currentLvlNodes = Seq(actualTree)
          while (currentLvlNodes.head.lvl<lvl)
            currentLvlNodes = currentLvlNodes.flatMap(node=> node.children)
          currentLvlNodes.map(_.value)
      }
    }
    
    def SIF(
             affineTransformations:Seq[AffineTransform]
           ):LazyTree[AffineTransform] = {
      LazyTree(new AffineTransform())(
        at => affineTransformations.map {
          x =>
            val result = new AffineTransform(at)
            result.concatenate(x)
            result
        }
        )
    }
  }
  
  object myComponent extends JComponent{
    override protected def paintComponent( g: Graphics ): Unit = {
      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]

      g2d.draw(cut1)
      g2d.draw(cut2)
      g2d.draw(cut3)
    }
  }

//  jFrame.add(myComponent)
//  jFrame.add(
//    new JComponent{
//    override protected def paintComponent( g: Graphics ): Unit = {
//      super.paintComponent(g)
//      val g2d = g.asInstanceOf[Graphics2D]
//
//      g2d.transform(xT)
//
//      g2d.draw(cut1)
//      g2d.draw(cut2)
//      g2d.draw(cut3)
//    }
//  })
  val testZOLine = new JComponent {
  override def paintComponent( g: Graphics ): Unit = {
    super.paintComponent(g)
    val g2d = g.asInstanceOf[Graphics2D]
    g2d.draw(new Line2D.Double(0,10,40,10))
    g2d.setTransform(startTransposition)
    g2d.draw(new Line2D.Double(0,0,1,1))
  }
}
  jFrame.add(testZOLine)

  jFrame.revalidate()
}

