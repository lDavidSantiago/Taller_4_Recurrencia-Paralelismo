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

    val top = task(m1a.map(row => m2t.map(col => prodPunto(row, col))))
    val bot = task(m1b.map(row => m2t.map(col => prodPunto(row, col))))

    top.join() ++ bot.join()
  }

  def multMatrozRec(mtx1: Matriz, mtx2: Matriz): Matriz = {
    val n = mtx1.length
    val halfN = n / 2
    if (n == 1) {
      Vector(Vector(mtx1.head.head * mtx2.head.head))
    } else{
      val (a, c) = mtx1.splitAt(halfN)
      val (a1, b1) = a.map(_.splitAt(halfN)).unzip
      val (c1, d1) = c.map(_.splitAt(halfN)).unzip

      val (e, g) = mtx2.splitAt(halfN)
      val (e1, f1) = e.map(_.splitAt(halfN)).unzip
      val (g1, h1) = g.map(_.splitAt(halfN)).unzip

      val leftup = matrixAddition(multMatrozRec(a1, e1), multMatrozRec(b1, g1))
      val rightup = matrixAddition(multMatrozRec(a1, f1), multMatrozRec(b1, h1))
      val leftlow = matrixAddition(multMatrozRec(c1, e1), multMatrozRec(d1, g1))
      val rightlow = matrixAddition(multMatrozRec(c1, f1), multMatrozRec(d1, h1))

      (leftup zip rightup map { case (x, y) => x ++ y }) ++ (leftlow zip rightlow map { case (x, y) => x ++ y })


      }
  }
  def matrixAddition(mtx1: Matriz, mtx2: Matriz): Matriz = {
    val n = mtx1.length
    Vector.tabulate(n, n)((i, j) => mtx1(i)(j) + mtx2(i)(j))
  }
  def matrixsubtraction(mtx1: Matriz, mtx2: Matriz): Matriz = {
    val n = mtx1.length
    Vector.tabulate(n, n)((i, j) => mtx1(i)(j) - mtx2(i)(j))
  }

  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    Vector.tabulate(l, l) { (row, col) => m(i + row)(j + col)
    }
  }

  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length

    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val newSize = n / 2
      val a = subMatriz(m1, 0, 0, newSize)
      val b = subMatriz(m1, 0, newSize, newSize)
      val c = subMatriz(m1, newSize, 0, newSize)
      val d = subMatriz(m1, newSize, newSize, newSize)
      val e = subMatriz(m2, 0, 0, newSize)
      val f = subMatriz(m2, 0, newSize, newSize)
      val g = subMatriz(m2, newSize, 0, newSize)
      val h = subMatriz(m2, newSize, newSize, newSize)

      val c11 = matrixAddition(multMatrizRec(a, e), multMatrizRec(b, g))
      val c12 = matrixAddition(multMatrizRec(a, f), multMatrizRec(b, h))
      val c21 = matrixAddition(multMatrizRec(c, e), multMatrizRec(d, g))
      val c22 = matrixAddition(multMatrizRec(c, f), multMatrizRec(d, h))

      val top = c11.zip(c12).map { case (row1, row2) => row1 ++ row2 }
      val bottom = c21.zip(c22).map { case (row1, row2) => row1 ++ row2 }
      top ++ bottom
    }
  }


}
