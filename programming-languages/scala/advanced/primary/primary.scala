/* Let us consider the possibility to parse and evaluate scripts representing
    additions and subtractions written as in the primary school.
123456 +
123456 +
123456 +
   469 =
--------
370837

999999999 +
       100 -
        99 =
------------
1000000000

789 -
 234567 +
  20100 +
 109822 -
   7070 =
---------
-110926

The rules to consider are:
    all the figures stand in each own row and are vertically right aligned
    operators are the last symbol in each row
    numbers can be negative
    only one = symbol is admitted and it is in the last row before the result
    the result is separated from the expression by a dashed row long as the longest row
        (operator included) and starting from the first column

The script includes both the operation and its expected result,
    the result of parsing/evaluation such a script should be the validation that the written result is the correct one.
 */

import java.nio.file.Paths
import scala.util.Try

def readlines(p: String) =
  val path = Paths.get(p).toAbsolutePath()

  var lines = io.Source
    .fromFile(path.toString())
    .getLines()
    .toList
  lines

def tryToInt(s: String) = Try(s.toInt).toOption

def validateScript(file: String) =
  val lines = readlines(file)
  val result = lines.last.toInt

  def calculateResult(l: List[String], nextOp: String, acc: Int): Int =
    l match {
      case Nil => acc
      case head :: tail => {
        val splitted = head.trim.split(" ")
        val operation = splitted.last
        tryToInt(splitted(0)) match {
          case None => acc
          case Some(number) =>
            nextOp match {
              case "+" => calculateResult(tail, operation, acc + number)
              case "-" => calculateResult(tail, operation, acc - number)
              case "=" => calculateResult(tail, operation, acc - number)
            }
        }
      }
    }

  result == calculateResult(lines, "+", 0)

def validateScripts(file: List[String]): Unit = file match {
  case Nil => ()
  case head :: next => {
    assert(true == validateScript(head))
    validateScripts(next)
  }
}

@main def m(file: String*) =
  validateScripts(file.toList)
