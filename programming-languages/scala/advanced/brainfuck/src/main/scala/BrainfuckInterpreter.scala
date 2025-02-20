import Brainfuck.*

import scala.collection.mutable

object BrainfuckInterpreter {
  def exec(program: Program, env: Environment): Unit = {
    def _exec(expressions: List[Command], env: Environment): Unit = {
      expressions.foreach {
        case IncrementPointer() => env.incrementPointer()
        case DecrementPointer() => env.decrementPointer()
        case IncrementData() => env.increment()
        case DecrementData() => env.decrement()
        case Loop(innerExpressions) => while (env.get() > 0) _exec(innerExpressions, env)
        case Print() => print(env.get().toChar)
        case Input() => env.put(Console.in.read())
        case _ => throw new IllegalArgumentException()
      }
    }
    _exec(program.expressions, env)
  }
}


class Environment {

  private val data = new mutable.HashMap[Int, Int].withDefault(_ => 0)
  private var pointer = 0

  def incrementPointer(): Unit = pointer += 1

  def decrementPointer(): Unit = pointer -= 1

  def increment(): Unit = data(pointer) += 1

  def decrement(): Unit = data(pointer) -= 1

  def get(): Int = data(pointer)

  def put(n: Int): Unit = data(pointer) = n

  override def toString: String = f"^$pointer, " + data.toString()
}