package org.example

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NewtonDerivar extends AnyFunSuite {
  val metodoNewton = new MetodoNewton()

  test("Derivar expr2 con respecto a x") {
    val expr2 = Suma(Atomo('x'), Atomo('x'))
    assert(metodoNewton.mostrar(metodoNewton.derivar(expr2, Atomo('x'))) == "(1.0 + 1.0)", s"Test 2 failed")
  }

  test("Derivar expr3 con respecto a x") {
    val expr3 = Expo(Atomo('x'), Numero(3))
    assert(metodoNewton.mostrar(metodoNewton.derivar(expr3, Atomo('x'))) == "(3.0 * ((x ^ 2.0) * 1.0))", s"Test 3 failed")
  }

  test("Derivar una expresión compleja con respecto a x") {
    val expr = Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x')))
    assert(metodoNewton.mostrar(metodoNewton.derivar(expr, Atomo('x'))) == "(0.0 + ((0.0 * x) + (3.0 * 1.0)))")
  }

  test("Derivar expr6 con respecto a x") {
    val expr6 = Prod(Atomo('x'), Prod(Atomo('x'), Atomo('x')))  // x * x * x
    assert(metodoNewton.mostrar(metodoNewton.derivar(expr6, Atomo('x'))) == "((1.0 * (x * x)) + (x * ((1.0 * x) + (x * 1.0))))")
  }

  test("Derivar expr7 con respecto a y") {
    val expr7 = Prod(Atomo('x'), Atomo('x'))
    assert(metodoNewton.mostrar(metodoNewton.derivar(expr7, Atomo('y'))) == "((0.0 * x) + (x * 0.0))")
  }
}
