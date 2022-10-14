package CG

case class Vector2D(x:Double,y:Double) {
  def + (other:Vector2D):Vector2D = Vector2D(x + other.x,y+other.y)
  def - (other:Vector2D):Vector2D = Vector2D(x - other.x,y-other.y)
  def * (z:Double):Vector2D = Vector2D(x*z,y*z)
  def * (other:Vector2D):Double = x*other.x + y*other.y
  def / (z:Double):Vector2D = Vector2D(x/z,y/z)
  def length: Double = math.sqrt(x*x + y*y)
}
