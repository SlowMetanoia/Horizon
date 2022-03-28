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
val string = """dflgikjslkfg dfgkdjfgl //dfd//dfgdfg dfg// gfh fgh """
val noComm = """"""
val comment = noComm.split("//").tail.flatMap("//"+_.mkString).mkString