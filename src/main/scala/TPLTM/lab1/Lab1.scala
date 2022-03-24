package TPLTM.lab1

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
sealed trait Expr
case class ConstStr(value:String) extends Expr
case class Identifier(value:String,id:Int) extends Expr
case class Condition(op:String,left:Expr,right:Expr) extends Expr
case class Operator(op:String,left:Expr,right:Expr) extends Expr
case class ExprList(exprList:List[Expr]) extends Expr
case class DoWhile(condition: Condition,expr: Expr) extends Expr
case class IdTable(map:Map[Int,String])
class Lab1 extends RegexParsers{
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
  val strAllowedChars: Regex = """.*""".r
  def id:Parser[Identifier] = (identifier | identifier)^^{id=> Identifier(id,getId(id))}
  def strConst:Parser[ConstStr] = "\"" ~> strAllowedChars <~ "\"" ^^ConstStr
  def eq:Parser[Operator] = (id ~ "=" ~ (id | strConst)) ^^{
    case id ~ op ~ idOrValue => Operator(op,id,idOrValue)
  }
  def cond:Parser[Condition] = ((id|strConst)~("="|"<="|">=")~(id|strConst)) ^^{
    case idOrValue1 ~ op ~ idOrValue2 => Condition(op,idOrValue1,idOrValue2)
  }
  def dWhile:Parser[DoWhile] = "do" ~ expr ~ "while" ~ cond ^^{
    case _ ~ expr ~ _ ~ cond => DoWhile(cond,expr)
  }
  def expr:Parser[ExprList] = rep(eq | dWhile)^^ExprList
}
object Lab1 extends App {

}
