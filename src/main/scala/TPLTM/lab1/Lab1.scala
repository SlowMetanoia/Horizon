package TPLTM.lab1

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
 * Первая лаба по методам трансляции
 * Задание
 * Реализовать программу, выполняющая лексический анализ входного языка и создание
 * таблицы лексем с указанием их типов.
 * Программа должна выдавать сообщения о наличии во входном тексте ошибок, которые могут
 * быть обнаружены на этапе лексического анализа
 *
 * Вариант 17:
 * Входной язык содержит операторы цикла «do ... while (...)», разделённые символом
 * «;». Операторы условия содержат идентификаторы, знаки сравнения «<=», «=>», «=»,
 * строковые константы (последовательность символов в двойных кавычках), знак
 * присваивания «=».
 */
sealed trait Lexeme
case class ConstStr(value:String) extends Lexeme
case class Identifier(value:String,id:Int) extends Lexeme
case class Operator(op:String) extends Lexeme
case class KeyWord(kw:String) extends Lexeme
case class Comment(comm:String) extends Lexeme

class lexaaa
object lexaaa {
  def tokens(string: String): Seq[String] = {
    val comment = string.split("//").tail.flatMap("//"+_.mkString).mkString
    string.split(comment).head.split(" ") ++ Seq(comment)
  }
  
}
protected class Analyser extends RegexParsers{
  var idTable:Map[String,Int] = Map("$def"->0)
  def getId(name:String):Int =
    idTable.getOrElse(
      name,
      {
        idTable = idTable + ( name -> (idTable.values.max + 1))
        getId(name)
      }
      )
  val identifier: Regex = """[a-zA-Z_][\w]*""".r
  val strAllowedChars: Regex = """\".*\"""".r
  val comm: Regex = """//(.|s)*""".r
  def id:Parser[Identifier] = identifier ^^{id => Identifier(id,getId(id))}
  def strConst:Parser[ConstStr] = strAllowedChars ^^ConstStr
  def keyWord:Parser[KeyWord] = ("do" | "while") ^^KeyWord
  def operator:Parser[Operator] = ("="|"<="|">="|";"|"("|")")^^Operator
  def comment:Parser[Comment] = comm ^^Comment
  def lexemes:Parser[List[Lexeme]] = rep(comment|keyWord|id|strConst|operator)
  
}
object Analyser{
  def apply(): Analyser = new Analyser()
  def analyse(source: Source):(List[Lexeme],Map[String,Int]) = {
    val analyser = Analyser()
    (source.getLines().flatMap(string=> lexaaa.tokens(string))
           .flatMap(string=>
                      analyser.parseAll(analyser.lexemes,string)
                              .get)
           .toList
      ,analyser.idTable - "$def")
  }
}
//todo:комменты
object Lab1 extends App {
  val text = """a="ghj" do; fav =    "dfgdfg"; while(a<=b)""" + "\n"+"""ddd = // "asdasghgf""""
  val result = Analyser.analyse(Source.fromString(text))
  println(text)
  println(result._1.mkString("\n"))
}

/**
 * Отчёт:
 * теория
 *
 * листинг
 * скрины
 *
 * описание работы программы
 *
 */

/**
 * lab2
 * табличка id
 *
 */

