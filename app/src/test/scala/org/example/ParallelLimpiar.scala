package org.example

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParallelLimpiar extends AnyFunSuite{
  val metodoNewtonPar = new NewtonParelela()

  test("Limpiar expr1 con respecto a x") {
    val expr1 = Suma(Atomo('x'), Atomo('x'))
    assert(metodoNewtonPar.mostrar(metodoNewtonPar.limpiar(metodoNewtonPar.derivar(expr1, Atomo('x'),2))) == "2.0", s"Test 2 failed")
  }

  test("Limpiar expr2 con respecto a x") {
    val expr2 = Expo(Atomo('x'), Numero(3))
    assert(metodoNewtonPar.mostrar(metodoNewtonPar.limpiar(metodoNewtonPar.derivar(expr2, Atomo('x'),2))) == "(3.0 * (x ^ 2.0))", s"Test 3 failed")
  }

  test("Limpiar expr4 con respecto a x"){
    val expr3 =  Suma(Atomo('k'), Prod(Numero(3.0),Atomo('x')))
    assert(metodoNewtonPar.mostrar(metodoNewtonPar.limpiar(metodoNewtonPar.derivar(expr3, Atomo('x'),2))) == "3.0")
  }

  test("Limpiar expr5 con respecto a y"){
    val expr5 = Prod(Atomo('x'), Atomo('x'))
    assert(metodoNewtonPar.mostrar(metodoNewtonPar.limpiar(metodoNewtonPar.derivar(expr5,Atomo('y'),2))) == "0.0")
  }

  test("Limpiar expr6 con respecto a x"){
    val expr5 = Suma(Prod(Numero(2), Atomo('x')), Suma(Numero(0), Prod(Numero(1), Atomo('y')))) // 2 * x + (0 + 1 * y)
    assert(metodoNewtonPar.mostrar(metodoNewtonPar.limpiar(metodoNewtonPar.derivar(expr5, Atomo('x'),2))) == "2.0")
  }

  test("Derivar expr6 con respecto a x") {
    val expr6 = Prod(Atomo('x'), Logaritmo(Atomo('x')))
    assert(metodoNewtonPar.mostrar(metodoNewtonPar.limpiar(metodoNewtonPar.derivar(expr6, Atomo('x'),2))) == "((lg(x)) + (x * (1.0 / x)))")
  }

}
