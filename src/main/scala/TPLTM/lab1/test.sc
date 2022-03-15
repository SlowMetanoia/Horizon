import scala.io.Source
import scala.util.matching.Regex

val source = Source.fromFile("C:\\WorkData\\Scala_Projects\\Pet\\Horizon\\src\\main\\scala\\TPLTM\\lab1\\example.txt")

val bracketsPattern = """\{.*}""".r

val testStr = "{} {a} {aa} {dfsl;kdf;lgk} {\nasd}"
val testTextInString = source.getLines().mkString("\n")
bracketsPattern.findAllIn(testStr).toArray
bracketsPattern.findAllIn(source.getLines().mkString).toArray

val whileBlockPattern: Regex = """do.+while[\s]*\(.*\)""".r
val conditionPattern: Regex = """\(.*(=|=>|=<).*\)""".r
val identifierPattern: Regex = """^"\w+^"""".r
val assigmentPattern: Regex = """^(.*)=.*$""".r
val stringConstPattern: Regex = """".*"""".r

def applyPattern(pattern: Regex, text:String) = {
  pattern.findAllIn(text).toArray.mkString("\n")
}

val notWhile = testTextInString.split(whileBlockPattern.toString()).mkString("\n\n")

val whileBlocks = applyPattern(whileBlockPattern,testTextInString)

val strConstants = applyPattern(stringConstPattern,testTextInString)

class LexicalError(msg:String) extends Exception(msg)

sealed trait Lexeme

sealed trait Sign
object leq extends Sign{
  override def toString = "=<"
}
object geq extends Sign{
  override def toString = "=>"
}
object eq extends Sign{
  override def toString = "="
}

class expr(code:String)

case class StringConst(string: String) extends expr(string) with Lexeme
case class Identifier(identifier: String) extends expr(identifier) with Lexeme
case class Condition(left:expr,right:expr,sign:Sign) extends expr(left + " " + sign + " " + right) with Lexeme
case class Assignment(identifier:expr, value:expr) extends expr(identifier + "=" + value) with Lexeme
case class whileBlock(body:expr, cond: Condition) extends expr(s"do\n$body\nwhile($cond)") with Lexeme

object StringConst{
  def apply(string: String):StringConst = {
    if(stringConstPattern.findFirstIn(string).isDefined)
      new StringConst(string.subSequence(1,string.length-2).toString)
    else
      throw new LexicalError("wrong string const input")
  }
}

object Identifier{
  def apply(string: String):Identifier = {
    if(identifierPattern.findFirstIn(string).isDefined)

  }
}

stringConstPattern.findAllIn(testTextInString).toArray.mkString("\n")



