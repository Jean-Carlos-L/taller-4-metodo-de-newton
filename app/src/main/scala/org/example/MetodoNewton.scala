package org.example

trait Expr
case class Numero(d: Double) extends Expr
case class Atomo(x: Char) extends Expr
case class Suma(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Resta(e1: Expr, e2: Expr) extends Expr
case class Div(e1: Expr, e2: Expr) extends Expr
case class Expo(e1: Expr, e2: Expr) extends Expr
case class Logaritmo(e1: Expr) extends Expr

class MetodoNewton {
  def mostrar(e: Expr): String = e match {
    case Numero(d) => d.toString
    case Atomo(x) => x.toString
    case Suma(e1, e2) => s"(${mostrar(e1)} + ${mostrar(e2)})"
//    case Prod(Numero(1), e2) => mostrar(e2) // caso de 1 por algo
//    case Prod(e1, Numero(1)) => mostrar(e1) // caso de algo por un 1
    case Prod(e1, e2) => s"(${mostrar(e1)} * ${mostrar(e2)})"
    case Resta(e1, e2) => s"(${mostrar(e1)} - ${mostrar(e2)})"
    case Div(e1, e2) => s"(${mostrar(e1)} / ${mostrar(e2)})"
    case Expo(e1, e2) => s"(${mostrar(e1)} ^ ${mostrar(e2)})"
    case Logaritmo(e1) => s"(lg(${mostrar(e1)}))"
  }

  def derivar(f: Expr, a: Atomo): Expr = f match {
    case Numero(_) => Numero(0)
    case Atomo(x) if x == a.x => Numero(1)
    case Atomo(_) => Numero(0)
    case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
    case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
    case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Prod(e2, e2))
    case Expo(e1, Numero(n)) => Prod(Numero(n), Prod(Expo(e1, Numero(n - 1)), derivar(e1, a)))
    case Logaritmo(e1) => Div(derivar(e1, a), e1)
  }
  def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {
    case Numero(d) => d
    case Atomo(x) if x == a.x => v
    case Atomo(_) => throw new IllegalArgumentException("Variable desconocida")
    case Suma(e1, e2) => evaluar(e1, a, v) + evaluar(e2, a, v)
    case Resta(e1, e2) => evaluar(e1, a, v) - evaluar(e2, a, v)
    case Prod(e1, e2) => evaluar(e1, a, v) * evaluar(e2, a, v)
    case Div(e1, e2) => evaluar(e1, a, v) / evaluar(e2, a, v)
    case Expo(e1, e2) => Math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) => Math.log(evaluar(e1, a, v))
  }

  def limpiar(f: Expr): Expr = f match {
    case Suma(Numero(0), e2) => limpiar(e2)
    case Suma(e1, Numero(0)) => limpiar(e1)
    case Suma(Numero(d1), Numero(d2)) => Numero(d1 + d2)
    case Resta(e1, Numero(0)) => limpiar(e1)
    case Resta(e1, e2) if e1 == e2 => Numero(0)
    case Prod(_, Numero(0)) => Numero(0)
    case Prod(Numero(1), e2) => limpiar(e2)
    case Prod(Numero(0), _) => Numero(0)
    case Prod(e1, Numero(1)) => limpiar(e1)
    case Prod(Numero(d1), Numero(d2)) => Numero(d1 * d2)
    case Div(e1, Numero(1)) => limpiar(e1)
    case Div(Numero(0), _) => Numero(0)
    case Div(Numero(d1), Numero(d2)) if d1 == d2 => Numero(1)
    case Expo(_, Numero(0)) => Numero(1)
    case Expo(Numero(1), _) => Numero(1)
    case Logaritmo(Numero(1)) => Numero(0)
    case _ => f
  }
}
