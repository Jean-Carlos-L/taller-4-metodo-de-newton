/*
 * This source file was generated by the Gradle 'init' task
 */
package org.example

import org.scalameter._

import scala.util.Random

object Benchmarking {

  val metodoNewtonPar = new NewtonParelela()
  val metodoNewtonSeq = new MetodoNewton()

  // Crear una expresión completa para evaluar de 10 términos
  val expr = Resta(
    Resta(
      Resta(
        Resta(
          Resta(
            Resta(
              Resta(
                Resta(
                  Resta(
                    Resta(
                      Resta(
                        Resta(
                          Resta(
                            Resta(
                              Resta(
                                Resta(
                                  Resta(
                                    Resta(
                                      Resta(
                                        Resta(
                                          Resta(
                                            Resta(
                                              Resta(
                                                Resta(
                                                  Resta(
                                                    Resta(
                                                      Resta(
                                                        Resta(
                                                          Resta(
                                                            Resta(
                                                              Resta(
                                                                Resta(
                                                                  Resta(
                                                                    Resta(
                                                                      Resta(
                                                                        Resta(
                                                                          Resta(
                                                                            Resta(
                                                                              Resta(
                                                                                Resta(
                                                                                  Resta(
                                                                                    Resta(
                                                                                      Resta(
                                                                                        Resta(
                                                                                          Resta(
                                                                                            Resta(
                                                                                              Resta(
                                                                                                Resta(
                                                                                                  Resta(
                                                                                                    Resta(
                                                                                                      Resta(
                                                                                                        Resta(
                                                                                                          Resta(
                                                                                                            Resta(
                                                                                                              Resta(
                                                                                                                Resta(
                                                                                                                  Prod(
                                                                                                                    Prod(
                                                                                                                      Atomo('x'),
                                                                                                                      Atomo('x')
                                                                                                                    ),
                                                                                                                    Atomo('x')
                                                                                                                  ),
                                                                                                                  Prod(
                                                                                                                    Numero(6.0),
                                                                                                                    Prod(
                                                                                                                      Atomo('x'),
                                                                                                                      Atomo('x')
                                                                                                                    )
                                                                                                                  )
                                                                                                                ),
                                                                                                                Prod(
                                                                                                                  Numero(11.0),
                                                                                                                  Atomo('x')
                                                                                                                )
                                                                                                              ),
                                                                                                              Numero(6.0)
                                                                                                            ),
                                                                                                            Numero(5.0)
                                                                                                          ),
                                                                                                          Numero(4.0)
                                                                                                        ),
                                                                                                        Numero(3.0)
                                                                                                      ),
                                                                                                      Numero(2.0)
                                                                                                    ),
                                                                                                    Numero(1.0)
                                                                                                  ),
                                                                                                  Numero(0.0)
                                                                                                ),
                                                                                                Numero(6.0)
                                                                                              ),
                                                                                              Numero(5.0)
                                                                                            ),
                                                                                            Numero(4.0)
                                                                                          ),
                                                                                          Numero(3.0)
                                                                                        ),
                                                                                        Numero(2.0)
                                                                                      ),
                                                                                      Numero(1.0)
                                                                                    ),
                                                                                    Numero(0.0)
                                                                                  ),
                                                                                  Numero(6.0)
                                                                                ),
                                                                                Numero(5.0)
                                                                              ),
                                                                              Numero(4.0)
                                                                            ),
                                                                            Numero(3.0)
                                                                          ),
                                                                          Numero(2.0)
                                                                        ),
                                                                        Numero(1.0)
                                                                      ),
                                                                      Numero(0.0)
                                                                    ),
                                                                    Numero(6.0)
                                                                  ),
                                                                  Numero(5.0)
                                                                ),
                                                                Numero(4.0)
                                                              ),
                                                              Numero(3.0)
                                                            ),
                                                            Numero(2.0)
                                                          ),
                                                          Numero(1.0)
                                                        ),
                                                        Numero(0.0)
                                                      ),
                                                      Numero(6.0)
                                                    ),
                                                    Numero(5.0)
                                                  ),
                                                  Numero(4.0)
                                                ),
                                                Numero(3.0)
                                              ),
                                              Numero(2.0)
                                            ),
                                            Numero(1.0)
                                          ),
                                          Numero(0.0)
                                        ),
                                        Numero(6.0)
                                      ),
                                      Numero(5.0)
                                    ),
                                    Numero(4.0)
                                  ),
                                  Numero(3.0)
                                ),
                                Numero(2.0)
                              ),
                              Numero(1.0)
                            ),
                            Numero(0.0)
                          ),
                          Numero(6.0)
                        ),
                        Numero(5.0)
                      ),
                      Numero(4.0)
                    ),
                    Numero(3.0)
                  ),
                  Numero(2.0)
                ),
                Numero(1.0)
              ),
              Numero(0.0)
            ),
            Numero(6.0)
          ),
          Numero(5.0)
        ),
        Numero(4.0)
      ),
      Numero(3.0)
    ),
    Numero(2.0)
  )

  def benchmarkSequential(): Any = {
    val time = withWarmer {
      new Warmer.Default
    } measure {
      metodoNewtonSeq.raizNewton(expr, Atomo('x'), 9, metodoNewtonSeq.buenaAprox)
    }
    println(s"Tiempo promedio secuencial: ${time.value}")

  }

  def benchmarkParallel(): Any = {
    val time = withWarmer {
      new Warmer.Default
    } measure {
      metodoNewtonPar.raizNewton(expr, Atomo('x'), 9, metodoNewtonPar.buenaAprox)
    }
    println(s"Tiempo promedio paralelizacion: ${time.value}")
  }

  def main(args: Array[String]): Unit = {

    Benchmarking.benchmarkSequential()
    Benchmarking.benchmarkParallel()
  }

}