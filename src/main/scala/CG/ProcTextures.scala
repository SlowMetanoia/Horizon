package CG

import java.awt.geom.Rectangle2D
import java.awt.{ BasicStroke, Color, Graphics }
import javax.swing.{ JButton, JComponent, JLabel, JPanel }
import scala.swing.Graphics2D

object ProcTextures extends App{
  
  var gridColor = new Color(0,0,0)
  var cellColor = new Color(255,255,255)
  var outlineWidth = 3
  var cellSize = 10
  var brickHeight = 40
  
  case class RandomAdditions(f:(Int,Int)=>Boolean){
    def paintThings(x:Int,
                    y:Int,
                    w:Int,
                    h:Int,
                    g2d:Graphics2D,
                    color: Color): Unit = {
      g2d.setColor(color)
      for {i <- x to w + x
           j <- y to h + y}
        if (f(i, j)) drawPixel(i, j, g2d)
    }
  }
  def drawPixel(x:Int,y:Int,g2d:Graphics2D):Unit = g2d.drawLine(x,y,x,y)
  
  sealed trait TextureMode
  final object GRID extends TextureMode{
    //var additionsColor: Color = Color.green
    //var additions:RandomAdditions = RandomAdditions((_,_)=>false)
    var cellSize = 20
    var lineWidth = 3
    var gridColor: Color = Color.BLACK
    var cellColor: Color = Color.ORANGE
  }
  final object BRICKS extends TextureMode{
    var brickSizeX = 30
    var brickSizeY = 10
    var lineWidth = 3
    var gridColor: Color = Color.BLACK
    var brickColor: Color = Color.ORANGE
  }
  final object MULTICOLOR_GRID extends TextureMode{
    var cellSize = 20
    var lineWidth = 3
    var gridColor: Color = Color.BLACK
    var cellColors: Seq[Color] = Seq(
      Color.RED,Color.BLUE,Color.WHITE,Color.CYAN,Color.GREEN,Color.GRAY,Color.MAGENTA,Color.DARK_GRAY
    )
  }
  final object PLAIN_COLOR extends TextureMode{
    var color:Color = Color.YELLOW
  }

  object textureDrawer extends JComponent {
    var paintMe:Graphics2D=>Unit = _=>()
    setBounds(0, 5, 700, 670)
    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]
      paintMe(g2d)
      //g2d.setColor(Color.GRAY)
      //g2d.setStroke(new BasicStroke(4))
      //g2d.draw(getBounds)
    }
  }

  val jFrame = JFrameBasics.jFrame
  val jPanel = new JPanel()
  jPanel.setBounds(700,5,600,700)
  jPanel.add(new JLabel("Texture mode: \n"))
  val brickButton = new JButton("bricks")
  brickButton.addActionListener( _ => {
    mode = BRICKS
    drawTextures()
  })
  val plainColorButton = new JButton("fill with color")
  plainColorButton.addActionListener( _ => {
    mode = PLAIN_COLOR
    drawTextures()
  })
  val gridButton = new JButton("grid")
  gridButton.addActionListener( _ => {
    mode = GRID
    drawTextures()
  })
  val multicolorGridButton = new JButton("all the colors")
  multicolorGridButton.addActionListener( _ => {
    mode = MULTICOLOR_GRID
    drawTextures()
  })

  jPanel.add(brickButton)
  jPanel.add(plainColorButton)
  jPanel.add(gridButton)
  jPanel.add(multicolorGridButton)
  
  val gridColorSliders = Seq(
    new SliderInit(0,255,0,"Red",{value=> gridColor = new Color(value,gridColor.getGreen,gridColor.getBlue);textureDrawer.repaint()},true),
    new SliderInit(0,255,0,"Green",{value=> gridColor = new Color(gridColor.getRed,value,gridColor.getBlue);textureDrawer.repaint()},true),
    new SliderInit(0,255,0,"Blue",{value=> gridColor = new Color(gridColor.getRed,gridColor.getGreen,value);textureDrawer.repaint()},true)
    )
  val cellColorSliders = Seq(
    new SliderInit(0,255,cellColor.getRed,"Red",{value=> cellColor = new Color(value,cellColor.getGreen,cellColor.getBlue);textureDrawer.repaint()},true),
    new SliderInit(0,255,cellColor.getGreen,"Green",{value=> cellColor = new Color(cellColor.getRed,value,cellColor.getBlue);textureDrawer.repaint()},true),
    new SliderInit(0,255,cellColor.getBlue,"Blue",{value=> cellColor = new Color(cellColor.getRed,cellColor.getGreen,value);textureDrawer.repaint()},true)
  )
  val cellSettingsSliders = Seq(
    new SliderInit(5,100,cellSize,"Cell size",{value => cellSize = value;textureDrawer.repaint()},true),
    new SliderInit(1,10,outlineWidth,"Outline width",{value => outlineWidth = value;textureDrawer.repaint()},true),
    new SliderInit(5, 100, brickHeight, "Brick height",{ value => brickHeight = value; textureDrawer.repaint()}, true)
  )
  
  jPanel.add(slidersPanel(gridColorSliders,"Grid color settings"))
  jPanel.add(slidersPanel(cellColorSliders,"Cell color settings"))
  jPanel.add(slidersPanel(cellSettingsSliders,"Cell settings"))
  jPanel.revalidate()

  jFrame.setLayout(null)
  jFrame.add(textureDrawer)
  jFrame.add(jPanel)
  var mode: TextureMode = MULTICOLOR_GRID
  def drawTextures():Unit = {
    val square:(Double,Double)=>Rectangle2D = new Rectangle2D.Double(_,_,cellSize,cellSize)
    mode match {
      case PLAIN_COLOR =>
        textureDrawer.paintMe = g2d=> {
          g2d.setColor(cellColor)
          g2d.fill(textureDrawer.getBounds)
        }
      case GRID => textureDrawer.paintMe = g2d=> {

        val xs = series[Int](0)(_+cellSize).takeWhile(_<textureDrawer.getWidth)
        val ys = series[Int](0)(_+cellSize).takeWhile(_<textureDrawer.getHeight)
        g2d.setColor(cellColor)
        xs.foreach { x =>
          ys.foreach { y =>
            g2d.fill(square(x, y))
          }
        }
        g2d.setStroke(new BasicStroke(outlineWidth))
        g2d.setColor(gridColor)
        xs.foreach { x =>
          ys.foreach { y =>
            g2d.draw(square(x, y))
          }
        }
      }
      case MULTICOLOR_GRID => textureDrawer.paintMe = g2d=> {
        val xs = series[Int](0)(_ + cellSize).takeWhile(_ < textureDrawer.getWidth)
        val ys = series[Int](0)(_ + cellSize).takeWhile(_ < textureDrawer.getHeight)

        for{
          i<-xs.indices
          j<-ys.indices
        }{
          g2d.setColor(MULTICOLOR_GRID.cellColors((i+j)%MULTICOLOR_GRID.cellColors.length))
          g2d.fill(square(xs(i),ys(j)))
        }
        g2d.setColor(gridColor)
        g2d.setStroke(new BasicStroke(outlineWidth))
        for {
          i <- xs.indices
          j <- ys.indices
        } g2d.draw(square(xs(i),ys(j)))
      }
      case BRICKS => textureDrawer.paintMe = g2d=> {
        val rectangle:(Double,Double)=>Rectangle2D = new Rectangle2D.Double(_, _,cellSize, brickHeight)
        val xs = series[Int](0)(_+cellSize).takeWhile(_ < (textureDrawer.getWidth + cellSize))
        val ys = series[Int](0)(_+ brickHeight).takeWhile(_ < textureDrawer.getHeight)
        g2d.setColor(cellColor)
        for {
          i <- xs.indices
          j <- ys.indices
        } if ( j % 2 == 0) g2d.fill(rectangle(xs(i),ys(j))) else g2d.fill(rectangle(xs(i) - cellSize / 2,ys(j)))
        g2d.setColor(gridColor)
        g2d.setStroke(new BasicStroke(outlineWidth))
        for {
          i <- xs.indices
          j <- ys.indices
        } if (j % 2 == 0) g2d.draw(rectangle(xs(i), ys(j))) else g2d.draw(rectangle(xs(i) - cellSize / 2, ys(j)))
      }
    }
    textureDrawer.repaint()
  }
  drawTextures()
  jFrame.revalidate()
}