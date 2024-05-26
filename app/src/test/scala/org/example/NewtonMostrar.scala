package org.example

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NewtonMostrar extends AnyFunSuite {
  val metodoNewton = new MetodoNewton()

  test("Mostrar 5.0") {
    val test1 = Numero(5.0)
    assert(metodoNewton.mostrar(test1) == "5.0", s"Test 1 failed: ${metodoNewton.mostrar(test1)}")
  }

  test("Mostrar x") {
    val test2 = Atomo('x')
    assert(metodoNewton.mostrar(test2) == "x", s"Test 2 failed: ${metodoNewton.mostrar(test2)}")
  }

  test("Mostrar 3.0 + y") {
    val test3 = Suma(Numero(3.0), Atomo('y'))
    assert(metodoNewton.mostrar(test3) == "(3.0 + y)", s"Test 3 failed: ${metodoNewton.mostrar(test3)}")
  }

  test("Mostrar lg(z / 2.0)") {
    val test5 = Logaritmo(Div(Atomo('z'), Numero(2.0)))
    assert(metodoNewton.mostrar(test5) == "(lg((z / 2.0)))", s"Test 5 failed: ${metodoNewton.mostrar(test5)}")
  }

  test("mostrar(expr5) should return (((x + 2.0) / (x * x)) * (((x + 2.0) + ((x * x) ^ 5.0)) - (lg(x)))") {
    val expr1 = Suma(Atomo('x'), Numero(2))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    val expr3 = Suma(expr1, Expo(expr2, Numero(5)))
    val expr4 = Logaritmo(Atomo('x'))
    val expr5 = Prod(Div(expr1, expr2), Resta(expr3, expr4))
    val resultado = metodoNewton.mostrar(expr5)
    val esperado = "(((x + 2.0) / (x * x)) * (((x + 2.0) + ((x * x) ^ 5.0)) - (lg(x))))"
    assert(resultado == esperado, s"Expected: $esperado, but got: $resultado")
  }
}
