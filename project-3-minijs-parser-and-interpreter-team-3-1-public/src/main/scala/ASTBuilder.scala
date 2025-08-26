package edu.luc.cs.laufer.cs371.expressions

import util.Try
import Expr.*
import org.json4s.JNull
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL.*

object ASTBuilder extends ExprParser[Expr]:
  import Expr.*

  def repl: Parser[Expr] = rep(statement) ^^ (stmts => Block(stmts))

  override def onExpr: Expr ~ Option[String ~ Expr] => Expr =
    case l ~ None => l
    case l ~ Some("+" ~ r) => Plus(l, r)
    case l ~ Some("-" ~ r) => Minus(l, r)
    case _ => throw new MatchError("Unexpected expression structure")
  override def onTerm: Expr ~ Option[String ~ Expr] => Expr =
    case l ~ None => l
    case l ~ Some("*" ~ r) => Times(l, r)
    case l ~ Some("/" ~ r) => Div(l, r)
    case l ~ Some("%" ~ r) => Mod(l, r)
    case _ => throw new MatchError("Unexpected term structure")

  // Converts numeric literals to `Constant` nodes
  override def onNumber: String => Expr = Constant.apply compose (_.toInt)

  // Handles unary '+'
  override def onPlusFactor: Expr => Expr = identity

  // Handles unary '-'
  override def onMinusFactor: Expr => Expr = UMinus.apply

  // Handles parenthesized expressions
  override def onParenExpr: Expr => Expr = identity

  // Converts identifiers to `Variable` nodes
  override def onIdent: String => Expr = Variable.apply

  // Handles assignments
  override def onAssignment: (String, Expr) => Expr =
    (id, expr) => Assign(id, expr)

  // Handles conditionals with optional `else` blocks
  override def onConditional: (Expr, Expr, Option[Expr]) => Expr =
    (cond, thenBranch, elseBranch) =>
      Cond(cond, thenBranch, elseBranch.orElse(Some(Block(List()))))

  // Handles loops
  override def onLoop: (Expr, Expr) => Expr =
    (guard, body) => Loop(guard, body)

  // Handles blocks of statements
  override def onBlock: List[Expr] => Expr = stmts => Block(stmts)

end ASTBuilder