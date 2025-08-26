package edu.luc.cs.laufer.cs371.expressions
import org.scalatest.funsuite.AnyFunSuite
import Expr.*

class MiniJSTests extends AnyFunSuite:
// simple assingment test
    test("parse a simple assingment"){
        val input = "x = 5;"
        val result = ASTBuilder.parseAll(ASTBuilder.statement, input)
        println(s"Results: $result")
        assert(result.successful)
        assert(result.get == Assign("x", Constant(5)))
    }

// multiple assingment test
    test("Parse multiple assingments"){
        val input = "x = 5; y = 7;"
        val result = ASTBuilder.parseAll(ASTBuilder.repl, input)
        assert(result.successful)
        assert(result.get == Block(List(
            Assign("x", Constant(5)),
            Assign("y", Constant(7))
        )))
        
    }
// complex expression with variables test
    test("parse complex expression with vars"){
        val input = "((1 + y2) - (3 * y4)) / 5;"
        val result = ASTBuilder.parseAll(ASTBuilder.statement, input)
        println(s"result: $result")
        assert(result.successful)
        assert(result.get == Div(
            Minus(
                Plus(
                    Constant(1),
                    Variable("y2")
                ),
                Times(
                    Constant(3),
                    Variable("y4")

                )
            ),
            Constant(5)
        ))
    }
// testing parsing complex expression
    test("parse assingment with complex expression"){
        val input = "x = ((1 + y2) - (3 * y4)) / 5;"
        val result = ASTBuilder.parseAll(ASTBuilder.statement, input)
        println(s"Results: $result")
        assert(result.successful)
        assert(result.get == Assign("x",
            Div(
                Minus(
                    Plus(
                        Constant(1),
                        Variable("y2")
                    ),
                    Times(
                        Constant(3),
                        Variable("y4")
                    )
                ),
                Constant(5)
            )
        ))
    }
// Testing if statements
    test("Parse simple if statement"){
        val input = "if (1) { x = 2; }"
        val result = ASTBuilder.parseAll(ASTBuilder.statement, input)
        println(s"If statement result: $result")
        assert(result.successful)
        assert(result.get == Cond(
            Constant(1),
            Block(List(
                Assign("x", Constant(2))
            )),
            Some(Block(List()))
        ))
    }
// testing if-else statements
    test("Parse if-else"){
        val input = "if (1) { x = 2; } else { x = 3; }"
        val result = ASTBuilder.parseAll(ASTBuilder.statement, input)
        println(s"If-else statement result: $result")
        assert(result.successful)
        assert(result.get == Cond(
            Constant(1),
            Block(List(
                Assign("x", Constant(2))
            )),
            Some(Block(List(
                Assign("x", Constant(3))
            )))
        ))
    }
// testing multiple statements
    test("Parse block with multiple statements"){
        val input = "{ r = r + x; y = y + 1; }"
        val result = ASTBuilder.parseAll(ASTBuilder.statement, input)
        println(s"Block with multiple statements result: $result")
        assert(result.successful)
        assert(result.get == Block(List(
            Assign("r",
                Plus(Variable("r"), Variable("x"))
            ),
            Assign("y",
                Plus(Variable("y"), Constant(1))
            )
        )))
    }
// if statements with multiple statements test
    test("Parse if with multiple statements"){
        val input = "if (4) { r = r + x; y = y + 1; }"
        val result = ASTBuilder.parseAll(ASTBuilder.statement, input)
         println(s"If with multiple statements result: $result")
        assert(result.successful)
        assert(result.get == Cond(
            Constant(4),
            Block(List(
                Assign("r",
                Plus(Variable("r"), Variable("x"))
                ),
                Assign("y",
                    Plus(Variable("y"), Constant(1))
                )
            )),
            Some(Block(List()))
        ))
    }
// testing while loop
    test("Parse while loop") {
    val input = "while (y) { r = r + x; y = y - 1; }"
    val result = ASTBuilder.parseAll(ASTBuilder.statement, input)
    println(s"While loop result: $result")
    assert(result.successful)
    assert(result.get == Loop(
     Variable("y"),  // condition
        Block(List(     // body
         Assign("r", 
         Plus(Variable("r"), Variable("x"))
         ),
        Assign("y", 
         Minus(Variable("y"), Constant(1))
        )
     ))
    ))
}
// working on some negative tests to purposefully not pass
    test("reject bad input for simple assertion"){
        val input = "x = 5" // no semi colon
        val result = ASTBuilder.parseAll(ASTBuilder.statement, input)
        assert(!result.successful)

    }
    test("reject bad input for while loop"){
        val input = "while { x = 2; }"  // Wrong conditions
        val result = ASTBuilder.parseAll(ASTBuilder.statement, input)
        assert(!result.successful)
    }
    test("reject bad input for if statement"){
    val input = "if 1 { x = 2; }"
    val result = ASTBuilder.parseAll(ASTBuilder.statement, input)
    assert(!result.successful)
    }