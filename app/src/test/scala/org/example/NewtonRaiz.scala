package org.example

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NewtonRaiz extends AnyFunSuite {
  val metodoNewton = new MetodoNewton()

  test("Raiz de x^2 - 2 con x0 = 2.0") {
    val test1 = Resta(Prod(Atomo('x') ,Atomo('x')), Numero(2.0))
    assert(metodoNewton.raizNewton(test1, Atomo('x'), 2.0, metodoNewton.buenaAprox) == 1.414213562373095, s"Test 1 failed: ${metodoNewton.raizNewton(test1, Atomo('x'), 4.0, metodoNewton.buenaAprox)}")
  }

  test("Raiz de x^2 - 4 con x0 = 2.0") {
    val test2 = Resta(Prod(Atomo('x') ,Atomo('x')), Numero(4.0))
    assert(metodoNewton.raizNewton(test2, Atomo('x'), 2.0, metodoNewton.buenaAprox) == 2.0, s"Test 1 failed: ${metodoNewton.raizNewton(test2, Atomo('x'), 4.0, metodoNewton.buenaAprox)}")
  }

  test("Raiz de x^2 - 4 con x0 = 3.0") {
    val test3 = Resta(Prod(Atomo('x') ,Atomo('x')), Numero(4.0))
    assert(metodoNewton.raizNewton(test3, Atomo('x'), 3.0, metodoNewton.buenaAprox) == 2.0, s"Test 1 failed: ${metodoNewton.raizNewton(test3, Atomo('x'), 4.0, metodoNewton.buenaAprox)}")
  }

  test("Raiz de ((x * x) - 4) + (3 * x) con x0 = 2.0") {
    val test4 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0) ), Prod(Numero(3.0), Atomo('x')))
    assert(metodoNewton.raizNewton(test4, Atomo('x'), 2.0, metodoNewton.buenaAprox) == 1.0)
  }

  test("Raiz de x^3 - 6x^2 + 11x - 6 con x0 = 3.5") {
    val test5 = Resta(Resta(Resta(Prod(Prod(Atomo('x'), Atomo('x')), Atomo('x')), Prod(Numero(6.0), Prod(Atomo('x'), Atomo('x')))), Prod(Numero(11.0), Atomo('x'))), Numero(6.0))
    assert(metodoNewton.raizNewton(test5, Atomo('x'), 3.5, metodoNewton.buenaAprox) == 3.5, s"Test 5 failed: ${metodoNewton.raizNewton(test5, Atomo('x'), 3.5, metodoNewton.buenaAprox)}")
  }
}
