package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers

import Expr.*

trait ExprParser[Result] extends JavaTokenParsers:

  given [A, B](using CanEqual[A, A], CanEqual[B, B]): CanEqual[A ~ B, A ~ B] = CanEqual.derived

  // Modified expression parser to handle multiple terms
  def expr: Parser[Result] =
    term ~ rep(("+" | "-") ~ term) ^^ { 
      case first ~ rest => 
        rest.foldLeft(first) { 
          case (acc, op ~ term) => 
            // Create a structure that matches the expected type of onExpr
            onExpr(new ~(acc, Some(new ~(op, term))))
        }
    }

  def term: Parser[Result] = 
    factor ~ rep(("*" | "/" | "%") ~ factor) ^^ {
      case first ~ rest =>
        rest.foldLeft(first) {
          case (acc, op ~ fact) =>
            // Create a structure that matches the expected type of onTerm
            onTerm(new ~(acc, Some(new ~(op, fact))))
        }
    }
  // Factor parser
  def factor: Parser[Result] =
    wholeNumber ^^ onNumber |
    "+" ~> factor ^^ onPlusFactor |
    "-" ~> factor ^^ onMinusFactor |
    "(" ~> expr <~ ")" ^^ onParenExpr |
    ident ^^ onIdent

  // Statement parser
  def statement: Parser[Result] =
    expr <~ ";" ^^ (x => x) |
    assignment |
    conditional |
    loop |
    block

  // Assignment parser
  def assignment: Parser[Result] =
    ident ~ "=" ~ expr <~ ";" ^^ { case id ~ "=" ~ e => onAssignment(id, e) }

  // Conditional parser
  def conditional: Parser[Result] =
    "if" ~ "(" ~ expr ~ ")" ~ block ~ ("else" ~ block).? ^^ {
      case _ ~ _ ~ cond ~ _ ~ ifBlk ~ elseBlk => 
        onConditional(cond, ifBlk, elseBlk.map { case _ ~ result => result })
    }

  def loop: Parser[Result] =
    "while" ~ "(" ~ expr ~ ")" ~ block ^^ {
      case _ ~ _ ~ cond ~ _ ~ body =>
        onLoop(cond, body)
    }
  // Block parser
  def block: Parser[Result] =
    "{" ~> rep(statement) <~ "}" ^^ onBlock

  // Add repl as an abstract method in the trait
  def repl: Parser[Result]

  // Abstract hooks for subclasses
  def onExpr: (Result ~ Option[String ~ Result]) => Result
  def onTerm: (Result ~ Option[String ~ Result]) => Result
  def onNumber: String => Result
  def onPlusFactor: Result => Result
  def onMinusFactor: Result => Result
  def onParenExpr: Result => Result
  def onIdent: String => Result 
  def onAssignment: (String, Result) => Result
  def onConditional: (Result, Result, Option[Result]) => Result
  def onLoop: (Result, Result) => Result
  def onBlock: List[Result] => Result