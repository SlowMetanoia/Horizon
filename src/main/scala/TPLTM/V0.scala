package TPLTM

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
 *
 * Третья лаба по методам трансляции:
 * Построение дерева разбора.
 */
object V0 extends App {
  case class AnaliseResult(tree: ExpressionList, idMap: Map[String, Int])

  //Это необходимые для анализа классы символов
  sealed trait Lexeme

  sealed trait Value extends Lexeme

  sealed trait Expr extends Value

  case class ConstStr(value: String) extends Value

  case class Identifier(value: String, id: Int) extends Value

  case class Expression(left: Value, op: String, right: Value) extends Expr

  case class Condition(left: Value, op: String, right: Value) extends Expr

  case class ExpressionList(exprs: List[Expression]) extends Lexeme

  case class While(cond: Condition, body: ExpressionList) extends Lexeme

  val r: Regex = """//(.|s)*""".r

  //праздник костылей
  //для высечения while
  sealed trait LN

  case class CodeLine(string: String)

  case class CodeLines(strings: List[String]) extends LN

  case class WL(condition: String, body: CodeLines) extends LN

  case class WhileDivision(codeBlocks: List[LN]) extends LN

  //Это убирает комменты
  object DeCommenter {
    val comm = """//(.|s)*"""

    def apply(strings: Iterable[String]): Iterable[String] =
      strings.flatMap(str => str.split(comm))
  }

  //Это лексер.
  object Lexer {
    def apply(strings: Iterable[String]): Iterable[String] =
      strings.flatMap(_.split(""";|\s""")
        .collect { case str: String if str != "" => str })
  }

  //для высечения while
  object DeWhile {
    object WhileParser extends RegexParsers {
      private val linePattern = """(.*\n|\n.*)""".r

      def line: Parser[CodeLine] = linePattern ^^ CodeLine

      def lines: Parser[CodeLines] = rep(line) ^^ { lines => CodeLines(lines.map(_.string)) }

      def wl: Parser[WL] = "do" ~> lines ~ """while\s*\(""".r ~ """.*""" <~ ")" ^^ {
        case lines ~ _ ~ cond => WL(cond, lines)
      }

      def anything: Parser[WhileDivision] = rep(wl | lines) ^^ WhileDivision
    }

    def apply(strings: Iterable[String]): WhileDivision = {
      WhileParser.parseAll(WhileParser.anything, strings.mkString("\n")).get
    }
  }


  //Это такое синтаксическое дерево(курильщика)
  class SyntaxTree extends RegexParsers {
    var idTable: Map[String, Int] = Map("$def" -> 0)

    def getId(name: String): Int =
      idTable.getOrElse(
        name,
        {
          idTable = idTable + (name -> (idTable.values.max + 1))
          getId(name)
        }
      )

    val identifier: Regex = """[a-zA-Z_][\w]*""".r
    val constString: Regex = """\".*\"""".r

    //парсим разные виды значений
    def id: Parser[Identifier] = identifier ^^ { id => Identifier(id, getId(id)) }

    def strConst: Parser[ConstStr] = constString ^^ ConstStr

    def value: Parser[Value] = id | strConst

    //парсим выражения присваивания
    def expr: Parser[Expression] = value ~ "=" ~ value ^^ {
      case l ~ _ ~ _ if l.isInstanceOf[ConstStr] => throw new Exception("can`t assign to a value!")
      case l ~ op ~ r => Expression(l, op, r)
    }

    //парсим условия
    def cond: Parser[Condition] = value ~ (">=" | "<=" | "==") ~ value ^^ {
      case l ~ op ~ r => Condition(l, op, r)
    }

    //парсим подряд идущие выражения
    def exprList: Parser[ExpressionList] = rep(expr) ^^ ExpressionList

    //парсим do ... while
    def whl: Parser[While] = "do" ~> exprList ~ "while(" ~ cond <~ ")" ^^ {
      case body ~ _ ~ cond => While(cond, body)
    }

  }

  object SyntaxTree {
    def apply(): SyntaxTree = new SyntaxTree()

    def analyze(source: Source): AnaliseResult = {
      val tree = SyntaxTree()
      val pureText =
        Lexer(
          DeCommenter(
            source.getLines().toList)
        ).mkString

      println(pureText)
      val result = tree.parseAll(tree.exprList, pureText).get
      AnaliseResult(result, tree.idTable)
    }
  }

  val commTest = "some code //some comment\n" +
    "some other code\n" +
    "//some other comment\n ..."
  val lexerTest = "opr1 opr2  opr3     opr4\n" +
    "opr5 opr6     opr7"
  //println(DeCommenter(commTest.split("\n")))
  //println(Lexer(lexerTest.split("\n")))
  val generalTest = """a="ghj" do; fav =    "dfgdfg"; while(a<=b)//dfgdfg""" + "\n" + """ddd = ll// "asdasghgf""""
  //val result = SyntaxTree.analyze(Source.fromString(generalTest))
  //println(result)
  val dclGeneralTest = Lexer(DeCommenter(generalTest.split("\n")))
  println(dclGeneralTest)
  val ddclGeneralTest = DeWhile(dclGeneralTest)
}