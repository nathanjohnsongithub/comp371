package edu.luc.cs.laufer.cs371.expressions

enum Expr derives CanEqual:
  // Existing cases...
  case Constant(value: Int)
  case UMinus(expr: Expr)
  case Plus(left: Expr, right: Expr)
  case Minus(left: Expr, right: Expr)
  case Times(left: Expr, right: Expr)
  case Div(left: Expr, right: Expr)
  case Mod(left: Expr, right: Expr)
  
  // New cases
  case Assign(variable: String, value: Expr)
  case Cond(condition: Expr, ifBlock: Expr, elseBlock: Option[Expr])
  case Loop(condition: Expr, body: Expr)
  case Block(statements: List[Expr])

  // More specific expressions
  case Variable(name: String)  // For variables in expressions