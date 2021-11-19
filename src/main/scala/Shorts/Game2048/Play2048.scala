package Shorts.Game2048
import java.awt.event.KeyEvent
import java.awt
import scala.annotation.tailrec
import scala.util.Random

object Play2048 extends App {
  val n = 4
  type fld = Array[Array[Int]]
  var field = for(i<-(1 to n).toArray) yield for(j<-(1 to n).toArray) yield 0
  @tailrec
  def randomFreePlace:(Int,Int) = {
    val rand = Random
    val (x,y) = (rand.nextInt(n),rand.nextInt(n))
    if(field(x)(y)==0) (x,y) else randomFreePlace
  }
  def swap(a:(Int,Int),b:(Int,Int)) = {
    val c = field(a._1)
  }
  def isAnySpaceFree:Boolean = field.exists(_.exists(_==0))
  def generateTile():Unit = {
    val (x,y) = randomFreePlace
    field(x)(y) = if(Random.nextInt(11)==10) 4 else 2
  }
  def moveLeft():Unit = {
    field.indices.foreach {i=>field.indices.foreach {
      case 0 =>
    } }
  }
}
