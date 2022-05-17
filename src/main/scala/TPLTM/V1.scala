package TPLTM

import scala.util.parsing.combinator.RegexParsers

object V1 extends App{

  case class Cond(str:String)
  //Это убирает комменты
  def deComment(strings: Iterable[String]): Iterable[String] =
    strings.flatMap(str => str.split("""//(.|s)*"""))

  //Это лексер.
  def lexAnalyze(strings: Iterable[String]): Iterable[String] =
    strings.flatMap(_.split(""";|\s""")
      .collect { case str: String if str != "" => str })

  val whilePattern = """do[\s\n.]*while[\s\n]*([\s.]*)""".r

  case class WHILE(condition:String,body:List[String])
  //выпиливание мусора из do while
  object WhileParser extends RegexParsers{
    def doo:Parser[Unit] ="""\s*do\s*""".r^^{ _ => () }
    def wwl:Parser[Unit] = """\s*while\s*""".r^^{_ => ()}
    def cond:Parser[String] =
      """\s*(\s*.*\s*)\s*""".r ^^{str =>
      val str1 = str.drop(str.indexOf("(")+1)
      str1.take(str1.lastIndexOf(")"))
    }
    def weeweewhile:Parser[WHILE] = doo ~> """([\s\n]*.*[\s\n]*)*\s*while""".r ~ cond^^{
      case list ~ cond => WHILE(cond,list.take(list.indexOf("while")).split("\n").toList)
    }
  }
  val conditionTest = "(ksjdhfskjdfh)"
  val doTest = "    do     "
  val whileTest = "while"
  val weeweeTest = "do       \n" +
    "jorgksjgh" +
    "   while       (skjdghxkjf)"

  val Result = WhileParser.parseAll(WhileParser.weeweewhile,weeweeTest).get
  println(Result)
  println(whilePattern.findAllIn(weeweeTest).toList)
}