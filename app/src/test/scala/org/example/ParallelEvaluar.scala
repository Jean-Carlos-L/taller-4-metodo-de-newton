package org.example

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParallelEvaluar extends AnyFunSuite{
  val metodoNewtonPar = new NewtonParelela()

  test("Evaluar 5.0") {
    val test1 = Numero(5.0)
    assert(metodoNewtonPar.evaluar(test1, Atomo('x'), 1.0, 0) == 5.0, s"Test 1 failed")
  }

  test("Evaluar Suma ( expr1,expr2 ) ,Atomo ( ’x’ ) , 5.0"){
    val expr1 = Suma (Atomo('x'), Numero(2))
    val expr2 = Prod (Atomo('x'), Atomo('x'))
    val test2 = Suma(expr1, expr2)
    assert(metodoNewtonPar.evaluar(test2, Atomo('x'), 5.0, 2) == 32.0, s"Test 2 failed")
  }

  test("Evaluar division"){
    val expr1 = Suma(Atomo('x'), Numero(2))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    val test3 = Div(expr1, expr2)
    assert(metodoNewtonPar.evaluar(test3, Atomo('x'), 5.0, 2) == 0.28)
  }

  test("evaluar potencia"){
    val expr1 = Suma(Atomo('x'), Numero(2))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    val test6 = Expo(expr1, expr2)
    assert(metodoNewtonPar.evaluar(test6, Atomo('x'), 5.0, 2) == 1.341068619663965E21)
  }

  test("Evaluar Logaritmo(expr1), Atomo('x'), 5.0"){
    val expr1 = Suma(Atomo('x'), Numero(2))
    val test7 = Logaritmo(expr1)
    assert(metodoNewtonPar.evaluar(test7, Atomo('x'), 5.0, 2) == 1.9459101490553132)
  }

}
