package org.example

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NewtonLimpiar extends AnyFunSuite {
  val metodoNewton = new MetodoNewton()

  test("Derivar expr1 con respecto a x") {
    val expr1 = Suma(Atomo('x'), Atomo('x'))
    assert(metodoNewton.mostrar(metodoNewton.limpiar(metodoNewton.derivar(expr1, Atomo('x')))) == "(1.0 + 1.0)", s"Test 2 failed")
  }

  test("Derivar expr2 con respecto a x") {
    val expr2 = Expo(Atomo('x'), Numero(3))
    assert(metodoNewton.mostrar(metodoNewton.limpiar(metodoNewton.derivar(expr2, Atomo('x')))) == "(3.0 * ((x ^ 2.0) * 1.0))", s"Test 3 failed")
  }

  test("Derivar expr4 con respecto a x"){
    val expr3 =  Suma(Atomo('k'), Prod(Numero(3.0),Atomo('x')))
    assert(metodoNewton.mostrar(metodoNewton.limpiar(metodoNewton.derivar(expr3, Atomo('x')))) == "3.0")
  }

  test("Derivar expr5 con respecto a y"){
    val expr5 =Prod(Atomo('x'), Atomo('x'))
    assert(metodoNewton.mostrar(metodoNewton.limpiar(metodoNewton.derivar(expr5,Atomo('y'))))== "0.0")
  }
}
