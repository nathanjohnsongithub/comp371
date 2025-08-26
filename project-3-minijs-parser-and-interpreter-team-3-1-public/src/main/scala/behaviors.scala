package edu.luc.cs.laufer.cs371.expressions

import util.Try
import Expr.*
import org.json4s.JNull

object behaviors:
  private def evaluateR(e: Expr): Int = e match
    case Constant(c) => c
    case UMinus(r)   => -evaluateR(r)
    case Plus(l, r)  => evaluateR(l) + evaluateR(r)
    case Minus(l, r) => evaluateR(l) - evaluateR(r)
    case Times(l, r) => evaluateR(l) * evaluateR(r)
    case Div(l, r)   => evaluateR(l) / evaluateR(r)
    case Mod(l, r)   => evaluateR(l) % evaluateR(r)
    case Variable(_) => throw new UnsupportedOperationException("Variables cannot be evaluated directly.")
    case Assign(_, r) => evaluateR(r) // Assuming evaluation of the RHS
    case Cond(cond, thenBranch, elseBranch) =>
      if (evaluateR(cond) != 0) evaluateR(thenBranch) else evaluateR(elseBranch.getOrElse(Block(Nil)))
    case Loop(guard, body) =>
      var result = 0
      while (evaluateR(guard) != 0) {
        result = evaluateR(body)
      }
      result
    case Block(statements) =>
      statements.map(evaluateR).lastOption.getOrElse(0) // Evaluate all statements, return the result of the last one

  def evaluate(e: Expr): Try[Int] = Try(evaluateR(e))

  def size(e: Expr): Int = e match
    case Constant(_) => 1
    case Variable(_) => 1
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)
    case Assign(_, v) => 1 + size(v)
    case Cond(cond, t, e) => 1 + size(cond) + size(t) + e.map(size).getOrElse(0)
    case Loop(cond, body) => 1 + size(cond) + size(body)
    case Block(statements) => statements.map(size).sum // Handle Block
    case UMinus(e) => 1 + size(e)

  def height(e: Expr): Int = e match
    case Constant(c) => 1
    case UMinus(r)   => 1 + height(r)
    case Plus(l, r)  => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r)   => 1 + math.max(height(l), height(r))
    case Mod(l, r)   => 1 + math.max(height(l), height(r))
    case Variable(_) => 1 // A variable has height 1
    case Assign(_, r) => 1 + height(r) // Assignment height depends on the RHS
    case Cond(cond, thenBranch, elseBranch) =>
      1 + math.max(height(cond), math.max(height(thenBranch), height(elseBranch.getOrElse(Block(Nil)))))
    case Loop(guard, body) => 1 + math.max(height(guard), height(body))
    case Block(statements) => 
      if (statements.isEmpty) 0 else 1 + statements.map(height).max

  import org.json4s.JsonAST.JValue
  import org.json4s.JsonDSL.*

  def toJson(e: Expr): JValue = e match
    case Constant(c) => ("type" -> "Constant") ~ ("value" -> c)
    case UMinus(r)   => ("type" -> "UMinus") ~ ("expr" -> toJson(r))
    case Plus(l, r)  => ("type" -> "Plus") ~ ("left" -> toJson(l)) ~ ("right" -> toJson(r))
    case Minus(l, r) => ("type" -> "Minus") ~ ("left" -> toJson(l)) ~ ("right" -> toJson(r))
    case Times(l, r) => ("type" -> "Times") ~ ("left" -> toJson(l)) ~ ("right" -> toJson(r))
    case Div(l, r)   => ("type" -> "Div") ~ ("left" -> toJson(l)) ~ ("right" -> toJson(r))
    case Mod(l, r)   => ("type" -> "Mod") ~ ("left" -> toJson(l)) ~ ("right" -> toJson(r))
    case Variable(name) => ("type" -> "Variable") ~ ("name" -> name)
    case Assign(v, expr) => 
      ("type" -> "Assign") ~ 
      ("variable" -> v) ~ 
      ("value" -> toJson(expr))
    case Cond(cond, ifBlock, elseBlock) =>
      ("type" -> "Cond") ~ 
      ("condition" -> toJson(cond)) ~ 
      ("if" -> toJson(ifBlock)) ~ 
      ("else" -> elseBlock.map(toJson).getOrElse(JNull))
    case Loop(cond, body) =>
      ("type" -> "Loop") ~ 
      ("condition" -> toJson(cond)) ~ 
      ("body" -> toJson(body))
    case Block(stmts) =>
      ("type" -> "Block") ~ 
      ("statements" -> stmts.map(toJson))

end behaviors