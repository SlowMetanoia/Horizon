package TPLTM.lab1

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class ArithmeticsParser extends RegexParsers{
  val number: Regex = "[0-9]+".r
  def expr: Parser[Int]  = term ~ opt(("+"|"-") ~ expr) ^^{
    case t ~ None => t
    case t ~ Some("+" ~ e)=> t + e
    case t ~ Some("-" ~ e)=> t - e
  }
  def term: Parser[Int]  = factor ~ rep("*" ~ factor)^^{
    case f ~ r => f * r.map(_._2).product
  }
  def factor:Parser[Int] = number ^^{_.toInt} | "(" ~ expr ~ ")" ^^{ case _ ~ e ~ _ => e}
}

object ArithmeticsParser extends App{
  val parser = new ArithmeticsParser
  val result = parser.parseAll(parser.expr,"(2+3)*5")
  println(result.get)
}
