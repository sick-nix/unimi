package Calculator
import scala.util.parsing.combinator._

class Calculator extends JavaTokenParsers {
  var variable_map = Map[String, Double]()
  def number = (floatingPointNumber | decimalNumber | wholeNumber) ^^ {
    _.toDouble
  }
  def char = """[A-Z]""".r ^^ { _.toString() }
  def variable_assignment = repsep(char, "=") ~ operation ^^ { case v ~ n =>
    v.foreach(v1 => variable_map += (v1 -> n))
  }
  def variable_access = char ^^ { case c =>
    variable_map.get(c) match {
      case Some(value) => value
      case None => throw new Exception("Access to unassigned variable " + c)
    }
  }
  def operator = "+" | "-" | "*" | "/" | "^"
  def fn = sqrt | sin | cos | tan
  def basic_expr =
    (number) ~ operator ~ (number) ^^ { case n1 ~ o ~ n2 =>
      o match {
        case "+" => n1 + n2
        case "-" => n1 - n2
        case "*" => n1 * n2
        case "/" => n1 / n2
        case "^" => Math.pow(n1, n2)
      }
    }
  def sqrt = "sqrt" ~> "(" ~> (basic_expr | number) <~ ")" ^^ { n =>
    Math.sqrt(n)
  }
  def sin = "sin" ~> "(" ~> (basic_expr | number) <~ ")" ^^ { n => Math.sin(n) }
  def cos = "cos" ~> "(" ~> (basic_expr | number) <~ ")" ^^ { n => Math.cos(n) }
  def tan = "tan" ~> "(" ~> (basic_expr | number) <~ ")" ^^ { n => Math.tan(n) }
  def operation = basic_expr | number | fn
  def operations = rep(variable_assignment | operation)
}
