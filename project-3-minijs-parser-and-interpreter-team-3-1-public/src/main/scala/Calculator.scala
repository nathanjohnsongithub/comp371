package edu.luc.cs.laufer.cs371.expressions
import scala.util.{Success, Failure}
import edu.luc.cs.laufer.cs371.expressions.Unparser.unparse
import Expr.*

object Calculator:

  def processExpr(input: String): Unit =
    println("Memory: " + Store.getState) // Initial store state
    println("You entered: " + input)
    val result = ASTBuilder.parseAll(ASTBuilder.repl, input)
    if result.isEmpty then
      println("This program could not be parsed")
    else
      import org.json4s.native.JsonMethods.{pretty, render}
      import behaviors._
      println("The result is : " + input)
      val raw = RawBuilder.parseAll(RawBuilder.repl, input).get
      println("The parsed statements are : " + raw)
      val expr = result.get
      println("The resulting program is: " + expr)
      println("The corresponding JSON structure is:")
      println(pretty(render(toJson(expr))))
      println("It has size " + size(expr) + " and height " + height(expr))
      println("The unparsed program is: ")
      println(unparse(expr))
      println("Program Evaluation: ")

      Store.evaluate(expr) match {
        case Success(value) => println(s"It evaluates to Success($value)")
        case Failure(e) => println(s"It evaluates to Failure(${e.getMessage})")
      }
      
      println("Memory: " + Store.getState) // Final store state


  def main(args: Array[String]): Unit =
    if args.length > 0 then
      processExpr(args mkString " ")
    else
      print("Enter MiniJS program: ")
      scala.io.Source.stdin.getLines() foreach:
        line =>
          processExpr(line)
          print("Enter MiniJS program: ")

end Calculator
