package taller_4_paralelismo_y_recursion
import scala.collection.parallel.CollectionConverters._
import common._

import java.util.concurrent.ForkJoinTask
import scala.util.Random
class Matrices{
  val random = new Random()
  type Matriz = Vector [ Vector [ Int ] ]


  //Crear matriz al azar
  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    val v = Vector.fill (long, long) {random.nextInt(vals)}
      v
  }


  //Crear Vectores al Azar
  def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
    val v = Vector.fill (long) {random.nextInt(vals)}
      v
  }

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => i * j }).sum
  }

  // Transpuesta de una matriz
  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  def multMatriz(m1: Matriz , m2: Matriz ) : Matriz = {
    val m2t = transpuesta(m2)
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => prodPunto(m1(i), m2t(j)))
  }

  def multMatrizParalelo(m1: Matriz, m2: Matriz): Matriz = {
    val m2t = transpuesta(m2)
    val (m1a, m1b) = m1.splitAt(m1.length / 2)

    val (top, bottom) = parallel(
      multMatriz(m1a, m2),
      multMatriz(m1b, m2)
    )
    top ++ bottom
  }


}
