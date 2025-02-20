import scala.io.Source
import java.io.File

object Brainfuck {

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      println("Source filename or brainfuck code required as argument")
      System.exit(0)
    }
    var source:String = ""
    if (File(args(0)).exists) {
      val src = Source.fromFile(args(0))
      source = src.getLines().mkString
      src.close()
    } else {
      source = args(0)
    }
    val program = BrainfuckParser.parse(source)
    if (program.successful) {
      BrainfuckInterpreter.exec(program.get, new Environment)
    } else {
      println("ERROR")
      println(program)
    }
  }

}