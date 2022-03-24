import scala.io.Source

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
val source = Source.fromFile("C:\\WorkData\\Scala_Projects\\Pet\\Horizon\\src\\main\\scala\\TPLTM\\lab1\\example.txt")
source.getLines()

object Lexer {
  def analyse( strings: Iterator[ String ] ): Iterator[ LexemeType ] =
    strings.flatMap(str=> str.split(";")).flatMap(analyseString)
  
  
  def analyseString( string: String ): Seq[ LexemeType ] = ???
  
  def findCond( string: String ): Seq[innerString] = {
    val regex = """\(.*\)""".r
    regex.findAllIn(string).toSeq.map(Condition).appended(pureStr(""))
                    .zip(
                      string.split(regex.toString()).map(pureStr)
                      )
                    .flatMap(pair=>Seq(pair._1,pair._2))
  }
}

sealed trait LexemeType

case class keyWord( value: String ) extends LexemeType
case class identifier( value: String ) extends LexemeType
case class operator( value: String ) extends LexemeType
case class value( value: String ) extends LexemeType

//object smthg extends LexemeType

sealed trait innerString

case class pureStr( value: String) extends AnyVal with innerString
case class Condition( value: String ) extends AnyVal with innerString

