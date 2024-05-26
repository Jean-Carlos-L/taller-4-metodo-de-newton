package org.example

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NewtonEvaluar extends AnyFunSuite {
  val metodoNewton = new MetodoNewton()

  test("Evaluar 5.0") {
    val test1 = Numero(5.0)
    assert(metodoNewton.evaluar(test1, Atomo('x'), 1.0) == 5.0, s"Test 1 failed")
  }

  test("Evaluar Suma ( expr1,expr2 ) ,Atomo ( ’x’ ) , 5.0"){
    val expr1 = Suma (Atomo('x'), Numero(2))
    val expr2 = Prod (Atomo('x'), Atomo('x'))
    val test2 = Suma(expr1, expr2)
    assert(metodoNewton.evaluar(test2, Atomo('x'), 5.0) == 32.0, s"Test 2 failed")
  }

  test("Evaluar division"){
    val expr1 = Suma(Atomo('x'), Numero(2))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    val test3 = Div(expr1, expr2)
    assert(metodoNewton.evaluar(test3, Atomo('x'), 5.0) == 0.28)
  }

  test("Evaluar expresion dividir"){
    val expr1 = Suma(Atomo('x'), Numero(2))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    val test4 = metodoNewton.mostrar(Div(expr1, expr2))
    assert(test4 == "((x + 2.0) / (x * x))")
  }

  test("Evaluar expresion de potencia"){
    val expr1 = Suma(Atomo('x'), Numero(2))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    val test5 = metodoNewton.mostrar(Expo(expr1, expr2))
    assert(test5 == "((x + 2.0) ^ (x * x))")
  }

  test("evaluar potencia"){
    val expr1 = Suma(Atomo('x'), Numero(2))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    val test6 = Expo(expr1, expr2)
    assert(metodoNewton.evaluar(test6, Atomo('x'), 5) == 1.341068619663965E21)
  }

  test("Evaluar Logaritmo(expr1), Atomo('x'), 5.0"){
    val expr1 = Suma(Atomo('x'), Numero(2))
    val test7 = Logaritmo(expr1)
    assert(metodoNewton.evaluar(test7, Atomo('x'), 5.0) == 1.9459101490553132)
  }

}
