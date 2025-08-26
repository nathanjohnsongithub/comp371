package edu.luc.cs.laufer.cs371.expressions
import org.scalatest.funsuite.AnyFunSuite
import edu.luc.cs.laufer.cs371.expressions.Store._
import scala.util.{Try, Success, Failure}
import Expr.*

class StoreTest extends AnyFunSuite:

    test("store basic") {
        Store.clear() // Ensure the store starts empty

        // Test setting a value in the store
        Store.set("x", Num(5))
        assert(Store.get("x").contains(Num(5)))

        // Test getting a non-existing value
        assert(Store.get("y").isEmpty)

        // Test updating a value in the store
        Store.set("x", Num(10))
        assert(Store.get("x").contains(Num(10)))

        // Test the clear function
        Store.clear()
        assert(Store.get("x").isEmpty)
    }

    test("store advanced - simple assignment and addition") {
        Store.clear() // Ensure the store starts empty
        val expr = Block(List(Assign("x", Constant(5)), Plus(Variable("x"), Constant(2))))
        val result = Store.evaluate(expr)

        result match {
            case Success(Num(value)) => assert(value == 7) // The result of x + 2 should be 7
            case Failure(exception) => fail(s"Unexpected failure: ${exception.getMessage}")
            case _ => fail("Unexpected result type")
        }

        Store.get("x") match {
            case Some(Store.Num(value)) => assert(value == 5) // x should hold the value 5
            case _ => fail("x was not stored correctly or has an incorrect type")
        }
    }

    test("store advanced - multiple assignments and operations") {
        Store.clear() // Ensure the store starts empty
        val expr = Block(List(
            Assign("x", Constant(10)),
            Assign("y", Constant(20)),
            Plus(Variable("x"), Variable("y"))
        ))
        val result = Store.evaluate(expr)

        result match {
            case Success(Store.Num(value)) => assert(value == 30) // x + y = 10 + 20
            case Failure(exception) => fail(s"Unexpected failure: ${exception.getMessage}")
            case _ => fail("Unexpected result type")
        }

        Store.get("x") match {
            case Some(Store.Num(value)) => assert(value == 10) // x should hold the value 10
            case _ => fail("x was not stored correctly or has an incorrect type")
        }

        Store.get("y") match {
            case Some(Store.Num(value)) => assert(value == 20) // y should hold the value 20
            case _ => fail("y was not stored correctly or has an incorrect type")
        }
    }

    test("store advanced - nested blocks and operations") {
        Store.clear() // Ensure the store starts empty
        val expr = Block(List(
            Assign("x", Constant(5)),
            Block(List(
                Assign("y", Plus(Variable("x"), Constant(3))),
                Times(Variable("y"), Constant(2))
            ))
        ))
        val result = Store.evaluate(expr)

        result match {
            case Success(Store.Num(value)) => assert(value == 16) // (x + 3) * 2 = 16
            case Failure(exception) => fail(s"Unexpected failure: ${exception.getMessage}")
            case _ => fail("Unexpected result type")
        }

        Store.get("x") match {
            case Some(Store.Num(value)) => assert(value == 5) // x should hold the value 5
            case _ => fail("x was not stored correctly or has an incorrect type")
        }

        Store.get("y") match {
            case Some(Store.Num(value)) => assert(value == 8) // y should hold the value (x + 3) = 8
            case _ => fail("y was not stored correctly or has an incorrect type")
        }
    }

    test("store advanced - conditionals") {
        Store.clear() // Ensure the store starts empty
        val expr = Block(List(Assign("x",Constant(10)), Cond(Variable("x"),Block(List(Assign("y",Constant(1)))),Some(Block(List())))))
        val result = Store.evaluate(expr)

        result match {
            case Success(Store.Num(value)) => assert(value == 0) // Condition x != 0, so y = 1
            case Failure(exception) => fail(s"Unexpected failure: ${exception.getMessage}")
            case _ => fail("Unexpected result type")
        }

        Store.get("x") match {
            case Some(Store.Num(value)) => assert(value == 10) // x should hold the value 10
            case _ => fail("x was not stored correctly or has an incorrect type")
        }

        Store.get("y") match {
            case Some(Store.Num(value)) => assert(value == 1) // y should hold the value 1
            case _ => fail("y was not stored correctly or has an incorrect type")
        }
    }

    test("store advanced - loops") {
        Store.clear() // Ensure the store starts empty
        val expr = Block(List(Assign("x",Constant(2)), Assign("y",Constant(3)), Assign("r",Constant(0)), Loop(Variable("y"),Block(List(Assign("r",Plus(Variable("r"),Variable("x"))), Assign("y",Minus(Variable("y"),Constant(1))))))))
        val result = Store.evaluate(expr)

        result match {
            case Success(Store.Num(value)) => assert(value == 0) // y = 5 after the loop
            case Failure(exception) => fail(s"Unexpected failure: ${exception.getMessage}")
            case _ => fail("Unexpected result type")
        }

        Store.get("r") match {
            case Some(Store.Num(value)) => assert(value == 6) // x should hold the value 0 after the loop
            case _ => fail("r was not stored correctly or has an incorrect type")
        }

        Store.get("x") match {
            case Some(Store.Num(value)) => assert(value == 2) // y should hold the value 5
            case _ => fail("x was not stored correctly or has an incorrect type")
        }
        
        Store.get("y") match {
            case Some(Store.Num(value)) => assert(value == 0) // y should hold the value 5
            case _ => fail("y was not stored correctly or has an incorrect type")
        }
    }