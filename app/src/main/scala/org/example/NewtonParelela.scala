package org.example

// importar la clase parallel de scala y la clase common

import common._


class NewtonParelela {
  /*
  * Esta clase implementa el método de Newton para calcular la raíz de una función de forma paralela
  * Los metodos que se paralelizaron fueron derivar, evaluar y limpiar
  * La estrategia utilizada es el paralelismo de tareas,  por ejemplo para el caso de la suma hacer que cada
  * derivaada se haga en un hilo con parallel() de scala, además agragarle un
  * umbral para definir cuando se debe paralelizar o no.
  */

  def mostrar(e: Expr): String = e match {
    case Numero(d) => d.toString
    case Atomo(x) => x.toString
    case Suma(e1, e2) => s"(${mostrar(e1)} + ${mostrar(e2)})"
    case Prod(e1, e2) => s"(${mostrar(e1)} * ${mostrar(e2)})"
    case Resta(e1, e2) => s"(${mostrar(e1)} - ${mostrar(e2)})"
    case Div(e1, e2) => s"(${mostrar(e1)} / ${mostrar(e2)})"
    case Expo(e1, e2) => s"(${mostrar(e1)} ^ ${mostrar(e2)})"
    case Logaritmo(e1) => s"(lg(${mostrar(e1)}))"
  }

  def derivar(f: Expr, a: Atomo, umbral: Int): Expr = f match {
    case Numero(_) => Numero(0)
    case Atomo(x) if x == a.x => Numero(1)
    case Atomo(_) => Numero(0)
    case Suma(e1, e2) =>
      if (shouldParallelize(f, umbral)) {
        val (d1, d2) = parallel(derivar(e1, a, umbral), derivar(e2, a, umbral))
        Suma(d1, d2)
      } else {
        Suma(derivar(e1, a, umbral), derivar(e2, a, umbral))
      }
    case Resta(e1, e2) =>
      if (shouldParallelize(f, umbral)) {
        val (d1, d2) = parallel(derivar(e1, a, umbral), derivar(e2, a, umbral))
        Resta(d1, d2)
      } else {
        Resta(derivar(e1, a, umbral), derivar(e2, a, umbral))
      }
    case Prod(e1, e2) =>
      if (shouldParallelize(f, umbral)) {
        val (d1, d2) = parallel(derivar(e1, a, umbral), derivar(e2, a, umbral))
        Suma(Prod(d1, e2), Prod(e1, d2))
      } else {
        Suma(Prod(derivar(e1, a, umbral), e2), Prod(e1, derivar(e2, a, umbral)))
      }
    case Div(e1, e2) =>
      if (shouldParallelize(f, umbral)) {
        val (d1, d2) = parallel(derivar(e1, a, umbral), derivar(e2, a, umbral))
        Div(Resta(Prod(d1, e2), Prod(e1, d2)), Prod(e2, e2))
      } else {
        Div(Resta(Prod(derivar(e1, a, umbral), e2), Prod(e1, derivar(e2, a, umbral))), Prod(e2, e2))
      }
    case Expo(e1, Numero(n)) => Prod(Numero(n), Prod(Expo(e1, Numero(n - 1)), derivar(e1, a, umbral)))
    case Logaritmo(e1) => Div(derivar(e1, a, umbral), e1)
  }

  // shouldParallelize: determina si se debe paralelizar una expresión o no
  def shouldParallelize(f: Expr, umbral: Int): Boolean = {
    countNodes(f) > umbral
  }

  // countNodes: cuenta el número de nodos de una expresión con el propósito de determinar si se debe paralelizar o no
  def countNodes(f: Expr): Int = f match {
    case Numero(_) => 1
    case Atomo(_) => 1
    case Suma(e1, e2) => 1 + countNodes(e1) + countNodes(e2)
    case Resta(e1, e2) => 1 + countNodes(e1) + countNodes(e2)
    case Prod(e1, e2) => 1 + countNodes(e1) + countNodes(e2)
    case Div(e1, e2) => 1 + countNodes(e1) + countNodes(e2)
    case Expo(e1, e2) => 1 + countNodes(e1) + countNodes(e2)
    case Logaritmo(e1) => 1 + countNodes(e1)
  }

  def evaluar(f: Expr, a: Atomo, v: Double, umbral: Int = 1000): Double = f match {
    case Numero(d) => d
    case Atomo(x) if x == a.x => v
    case Atomo(_) => throw new IllegalArgumentException("Variable desconocida")
    case Suma(e1, e2) =>
      if (shouldParallelize(f, umbral)) {
        val (res1, res2) = parallel(evaluar(e1, a, v, umbral), evaluar(e2, a, v, umbral))
        res1 + res2
      } else {
        evaluar(e1, a, v, umbral) + evaluar(e2, a, v, umbral)
      }
    case Resta(e1, e2) =>
      if (shouldParallelize(f, umbral)) {
        val (res1, res2) = parallel(evaluar(e1, a, v, umbral), evaluar(e2, a, v, umbral))
        res1 - res2
      } else {
        evaluar(e1, a, v, umbral) - evaluar(e2, a, v, umbral)
      }
    case Prod(e1, e2) =>
      if (shouldParallelize(f, umbral)) {
        val (res1, res2) = parallel(evaluar(e1, a, v, umbral), evaluar(e2, a, v, umbral))
        res1 * res2
      } else {
        evaluar(e1, a, v, umbral) * evaluar(e2, a, v, umbral)
      }
    case Div(e1, e2) =>
      if (shouldParallelize(f, umbral)) {
        val (res1, res2) = parallel(evaluar(e1, a, v, umbral), evaluar(e2, a, v, umbral))
        res1 / res2
      } else {
        evaluar(e1, a, v, umbral) / evaluar(e2, a, v, umbral)
      }
    case Expo(e1, e2) =>
      if (shouldParallelize(f, umbral)) {
        val (res1, res2) = parallel(evaluar(e1, a, v, umbral), evaluar(e2, a, v, umbral))
        Math.pow(res1, res2)
      } else {
        Math.pow(evaluar(e1, a, v, umbral), evaluar(e2, a, v, umbral))
      }
    case Logaritmo(e1) => Math.log(evaluar(e1, a, v, umbral))
  }


  def limpiar(f: Expr): Expr = f match {
    case Suma(Numero(0), e2) => limpiar(e2)
    case Suma(e1, Numero(0)) => limpiar(e1)
    case Suma(e1, e2) =>
      val (e1l, e2l) = parallel(limpiar(e1), limpiar(e2))
      (e1l, e2l) match {
        case (Numero(0), e2l) => e2l
        case (e1l, Numero(0)) => e1l
        case (Numero(d1), Numero(d2)) => Numero(d1 + d2)
        case (e1l, e2l) => Suma(e1l, e2l)
      }
    case Resta(e1, Numero(0)) => limpiar(e1)
    case Resta(e1, e2) if e1 == e2 => Numero(0)
    case Prod(_, Numero(0)) => Numero(0)
    case Prod(Numero(0), _) => Numero(0)
    case Prod(Numero(1), e2) => limpiar(e2)
    case Prod(e1, Numero(1)) => limpiar(e1)
    case Prod(e1, e2) =>
      val (e1l, e2l) = parallel(limpiar(e1), limpiar(e2))
      (e1l, e2l) match {
        case (Numero(0), _) => Numero(0)
        case (_, Numero(0)) => Numero(0)
        case (Numero(1), e2l) => e2l
        case (e1l, Numero(1)) => e1l
        case (Numero(d1), Numero(d2)) => Numero(d1 * d2)
        case (e1l, e2l) => Prod(e1l, e2l)
      }
    case Div(e1, Numero(1)) => limpiar(e1)
    case Div(Numero(0), _) => Numero(0)
    case Div(e1, e2) =>
      val (e1l, e2l) = parallel(limpiar(e1), limpiar(e2))
      (e1l, e2l) match {
        case (Numero(d1), Numero(d2)) if d1 == d2 => Numero(1)
        case (e1l, e2l) => Div(e1l, e2l)
      }
    case Expo(_, Numero(0)) => Numero(1)
    case Expo(Numero(1), _) => Numero(1)
    case Logaritmo(Numero(1)) => Numero(0)
    case _ => f
  }


  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
    def iter(xi: Double): Double = {
      if (ba(f, a, xi)) xi
      else {
        val fx = evaluar(f, a, xi)
        val dfx = evaluar(derivar(f, a, 10), a, xi)
        iter(xi - fx / dfx)
      }
    }

    iter(x0)
  }

  def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
    evaluar(f, a, d) < 0.0000000000000000000000000000000000000000000000000000000000000000000000000001
  }
}