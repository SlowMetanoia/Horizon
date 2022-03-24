package TPLTM.lab1

import scala.io.Source
import scala.util.matching.Regex
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
class Lexer
object Lexer {
  val whileBlockPattern: Regex = """do.*while\(.*\)""".r
  val conditionPattern: Regex = """\(.*(=|=>|=<).*\)""".r
  val identifierPattern: Regex = """^"\w+^"""".r
  val assigmentPattern: Regex = """^(.*)=.*""".r
  val stringConstPattern: Regex = """".*"""".r
  def analyse(from:Source) = {

    val text = from.getLines.mkString
  }
}
