package edu.luc.cs.laufer.cs371.expressions
import Expr.*

object Unparser {
  
  def unparse(expr: Expr, indent: Int = 0): String = expr match {
    case Constant(value) => value.toString
    case UMinus(e) => "-" + unparse(e, indent)
    case Plus(l, r) => unparse(l, indent) + " + " + unparse(r, indent)
    case Minus(l, r) => unparse(l, indent) + " - " + unparse(r, indent)
    case Times(l, r) => unparse(l, indent) + " * " + unparse(r, indent)
    case Div(l, r) => unparse(l, indent) + " / " + unparse(r, indent)
    case Mod(l, r) => unparse(l, indent) + " % " + unparse(r, indent)
    case Variable(name) => name
    case Assign(varName, e) => varName + " = " + unparse(e, indent) + ";"
    case Cond(condition, trueBlock, falseBlock) =>
      val elseBlock = falseBlock match {
        case Some(block) => s" else {\n${unparse(block, indent + 2)}\n}"
        case None => ""
      }
      s"if (${unparse(condition, indent)}) " +
        s"{\n${unparse(trueBlock, indent + 2)}\n}" + elseBlock
    case Loop(condition, block) =>
      s"while (${unparse(condition, indent)}) " +
        s"{\n${unparse(block, indent + 2)}\n}"
    case Block(statements) =>
      statements.map(unparse(_, indent + 2)).mkString("\n")
  }
}

