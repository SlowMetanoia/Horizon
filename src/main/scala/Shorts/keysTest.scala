package Shorts
import java.awt.event.KeyEvent

object keysTest extends App{
  def onLeft(keyEvent: KeyEvent) = {
    case KeyEvent.VK_KP_LEFT => println(123)
  }
}
