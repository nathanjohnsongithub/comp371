// package edu.luc.cs.laufer.cs371.expressions
// import org.scalatest.funsuite.AnyFunSuite


// class Test extends AnyFunSuite {
  
//   // Example test fixtures
//   val complex1 = Expr.Div(
//     Expr.Minus(
//       Expr.Plus(Expr.Constant(1), Expr.Constant(2)),
//       Expr.Times(Expr.Constant(3), Expr.Constant(4))
//     ),
//     Expr.Constant(5)
//   )
  
//   val complex1string = "((1 + 2) - (3 * 4)) / 5"
  
//   test("evaluate(p)") { assert(evaluate(complex1).get == -1) }
//   test("size(p)") { assert(size(complex1) == 9) }
//   test("height(p)") { assert(height(complex1) == 4) }
//   test("evaluate(q)") { assert(evaluate(complex2).get == 0) }
//   test("size(q)") { assert(size(complex2) == 10) }
//   test("height(q)") { assert(height(complex2) == 5) }
//   test("evaluate(bad)") { assert(evaluate(bad).isFailure) }

//   // Test unparse function
//   test("unparse(complex1)") {
//     val expectedOutput = "((1 + 2) - (3 * 4)) / 5"
//     val actualOutput = unparse(complex1)
//     assert(actualOutput == expectedOutput)
//   }

//   test("unparse(conditional)") {
//     val conditionalExpr = Expr.Cond(
//       Expr.Plus(Expr.Constant(1), Expr.Constant(2)),
//       Expr.Assign("x", Expr.Constant(3)),
//       Some(Expr.Assign("x", Expr.Constant(4)))
//     )
//     val expectedOutput = 
//       """if ((1 + 2)) {
//         |  x = 3;
//         |} else {
//         |  x = 4;
//         |}""".stripMargin
//     val actualOutput = unparse(conditionalExpr)
//     assert(actualOutput == expectedOutput)
//   }

//   test("unparse(loop)") {
//     val loopExpr = Expr.Loop(
//       Expr.Variable("x"),
//       Expr.Assign("y", Expr.Constant(10))
//     )
//     val expectedOutput = 
//       """while (x) {
//         |  y = 10;
//         |}""".stripMargin
//     val actualOutput = unparse(loopExpr)
//     assert(actualOutput == expectedOutput)
//   }
// }

// // Additional Test Cases
// // package edu.luc.cs.laufer.cs371.expressions

// class AdditionalTests extends AnyFunSuite:
//   test("parse simple assignment") {
//     val result = ASTBuilder.parseAll(ASTBuilder.statement, "x = 5;")
//     assert(result.successful)
//     assert(result.get == Assign("x", Constant(5)))
//   }

//   test("parse if statement") {
//     val result = ASTBuilder.parseAll(ASTBuilder.statement, "if (1) { x = 2; }")
//     assert(result.successful)
//     assert(result.get == Cond(Constant(1), Block(List(Assign("x", Constant(2)))), None))
//   }

//   test("parse while loop") {
//     val result = ASTBuilder.parseAll(ASTBuilder.statement, "while (x) { y = y + 1; }")
//     assert(result.successful)
//   }

//   test("parse block") {
//     val result = ASTBuilder.parseAll(ASTBuilder.statement, "{ x = 1; y = 2; }")
//     assert(result.successful)
//   }
