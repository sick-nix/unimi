import scala.util.parsing.combinator._
import Calculator.Calculator

case class WordFreq(word: String, count: Int) {
  override def toString = s"Word <$word> occurs with frequency $count"
}

class SimpleParser extends RegexParsers {
  def word: Parser[String] = """[a-z]+""".r ^^ { _.toString }
  def number: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def freq: Parser[WordFreq] = word ~ number ^^ { case wd ~ fr =>
    WordFreq(wd, fr)
  }
}

object TestSimpleParser extends Calculator {
  @main def main() = {
    // val ops = List("4+1", "5-2", "4+-3", "sin(1)", "sqrt(2+2)")
    val ops = List("A=B=1+3", "5+A", "1+B")
    parseAll(operations, ops.mkString("\n")) match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _)     => println(s"FAILURE: $msg")
      case Error(msg, _)       => println(s"ERROR: $msg")
    }
  }
}
