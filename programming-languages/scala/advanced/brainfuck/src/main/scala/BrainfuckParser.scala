import scala.util.matching.Regex
import scala.util.parsing.combinator.*

sealed trait Command

case class IncrementPointer() extends Command

case class DecrementPointer() extends Command

case class IncrementData() extends Command

case class DecrementData() extends Command

case class Print() extends Command

case class Input() extends Command

case class Loop(expressions: List[Command]) extends Command

case class Program(expressions: List[Command])


object BrainfuckParser extends JavaTokenParsers {
  //any character other than a command character is treated as whitespace
  override protected val whiteSpace: Regex = """[^><+\-\[\].,]*""".r

  private def loop: Parser[Loop] = "[" ~> rep(command) <~ "]" ^^ { expressions =>
    Loop(expressions)
  }

  private def command: Parser[Command] = ("<" | ">" | "+" | "-" | "." | "," | loop) ^^ {
    case ">" => IncrementPointer()
    case "<" => DecrementPointer()
    case "+" => IncrementData()
    case "-" => DecrementData()
    case "." => Print()
    case "," => Input()
    case Loop(expressions) => Loop(expressions)
  }

  def program: Parser[Program] = rep(command) ^^ { expressions => Program(expressions) }

  def parse(s: String): ParseResult[Program] = parseAll(program, s)
}