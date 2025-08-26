package edu.luc.cs.laufer.cs371.expressions

import scala.util.{Try, Success, Failure}
import Expr.*

object Store:
  // Mutable store for variables using Value type
  private var store = scala.collection.mutable.Map[String, Value]()

  // Get value of a variable
  def get(name: String): Option[Value] = 
    store.get(name)

  // Set value of a variable
  def set(name: String, value: Value): Unit = 
    store(name) = value

  // Clear the entire store
  def clear(): Unit = 
    store.clear()

  // Get current state of store as a string
  def getState: String = 
    s"Map(${store.map { case (k, v) => 
      v match {
        case Num(value) => s"$k -> Num($value)"
      }
    }.mkString(", ")})"

  // Define Result and Value
  type Result = Try[Value]
  sealed trait Value
  case class Num(value: Int) extends Value

  def evaluate(expr: Expr): Result = Try {
    expr match {
        // Constants evaluate to Num
        case Constant(v) => Num(v)

        // Variables retrieve their value from the store
        case Variable(name) =>
            Store.get(name).getOrElse(throw new NoSuchFieldException(s"java.lang.NoSuchFieldException: $name"))

        // Assignments update the store and evaluate to Num(0) (void)
        case Assign(name, value) =>
            val evaluatedValue = evaluate(value).get.asInstanceOf[Num]
            Store.set(name, evaluatedValue)
            Num(0)

        // Conditionals evaluate the chosen branch
        case Cond(cond, thenBranch, elseBranch) =>
            val condValue = evaluate(cond).get.asInstanceOf[Num].value
            if condValue != 0 then evaluate(thenBranch).get
            else evaluate(elseBranch.getOrElse(Block(List(Constant(0))))).get


        // Loops evaluate to Num(0)
        case Loop(guard, body) =>
            while evaluate(guard).get.asInstanceOf[Num].value != 0 do
                evaluate(body)
            Num(0)

        // Blocks evaluate to the last statement
        case Block(statements) =>
            statements.map(evaluate).last.get

        // Arithmetic operations
        case Plus(lhs, rhs) =>
            val l = evaluate(lhs).get.asInstanceOf[Num].value
            val r = evaluate(rhs).get.asInstanceOf[Num].value
            Num(l + r)

        case Minus(lhs, rhs) =>
            val l = evaluate(lhs).get.asInstanceOf[Num].value
            val r = evaluate(rhs).get.asInstanceOf[Num].value
            Num(l - r)

        case Times(lhs, rhs) =>
            val l = evaluate(lhs).get.asInstanceOf[Num].value
            val r = evaluate(rhs).get.asInstanceOf[Num].value
            Num(l * r)

        case Div(lhs, rhs) =>
            val l = evaluate(lhs).get.asInstanceOf[Num].value
            val r = evaluate(rhs).get.asInstanceOf[Num].value
            if r == 0 then throw new ArithmeticException("Division by zero")
            Num(l / r)

        case Mod(lhs, rhs) =>
            val l = evaluate(lhs).get.asInstanceOf[Num].value
            val r = evaluate(rhs).get.asInstanceOf[Num].value
            if r == 0 then throw new ArithmeticException("Modulo by zero")
            Num(l % r)

        // Unsupported expressions
        case _ => throw new UnsupportedOperationException(s"Unsupported expression: $expr")
      }
  }
  
end Store