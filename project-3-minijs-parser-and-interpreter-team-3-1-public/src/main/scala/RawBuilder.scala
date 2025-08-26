package edu.luc.cs.laufer.cs371.expressions
import Expr.*


object RawBuilder extends ExprParser[Any]:
  // Returns the left-hand side or combined result for expressions
  override def onExpr: (Any ~ Option[String ~ Any]) => Any = identity

  // Returns the left-hand side or combined result for terms
  override def onTerm: (Any ~ Option[String ~ Any]) => Any = identity

  // Returns the number as-is
  override def onNumber: String => Any = identity

  // Returns the factor as-is for unary '+'
  override def onPlusFactor: Any => Any = identity

  // Returns the negated factor as-is for unary '-'
  override def onMinusFactor: Any => Any = identity

  // Returns the inner expression as-is for parenthesized expressions
  override def onParenExpr: Any => Any = identity

  // Returns the identifier as-is
  override def onIdent: String => Any = identity

  // Returns the assignment as-is
  override def onAssignment: (String, Any) => Any = (id, expr) => (id, expr)

  // Returns the conditional as-is, including optional `else` branch
  override def onConditional: (Any, Any, Option[Any]) => Any = 
    (cond, thenBranch, elseBranch) => (cond, thenBranch, elseBranch)

  // Returns the loop as-is
  override def onLoop: (Any, Any) => Any = (guard, body) => (guard, body)

  // Returns the block as-is
  override def onBlock: List[Any] => Any = identity

  override def repl = rep(statement) ^^ identity

end RawBuilder