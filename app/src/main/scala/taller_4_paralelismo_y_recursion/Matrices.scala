package taller_4_paralelismo_y_recursion
import common._

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
  // Producto Punto
  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => i * j }).sum
  }

  // Producto Punto Paralelo
  def prodPuntoPar(v1: Vector[Int], v2: Vector[Int]): Int = {
    val (v1a, v1b) = v1.splitAt(v1.length / 2)
    val (v2a, v2b) = v2.splitAt(v2.length / 2)

    val sum1 = task {
      (v1a zip v2a).map({ case (i, j) => i * j }).sum
    }

    val sum2 = task {
      (v1b zip v2b).map({ case (i, j) => i * j }).sum
    }

    sum1.join() + sum2.join()
  }

  // Transpuesta de una matriz
  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }
// Multiplicacion estandar secuencial
  def multMatriz(m1: Matriz , m2: Matriz ) : Matriz = {
    val m2t = transpuesta(m2)
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => prodPunto(m1(i), m2t(j)))
  }
  // Version estandar paralela
  def multMatrizParalelo(m1: Matriz, m2: Matriz): Matriz = {
    val m2t = transpuesta(m2)
    val (m1a, m1b) = m1.splitAt(m1.length / 2)

    val top = task(m1a.map(row => m2t.map(col => prodPunto(row, col))))
    val bot = task(m1b.map(row => m2t.map(col => prodPunto(row, col))))

    top.join() ++ bot.join()
  }
  // Multiplicacion de matrices recursiva
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
  //Suma de matrices
  def matrixAddition(mtx1: Matriz, mtx2: Matriz): Matriz = {
    val n = mtx1.length
    Vector.tabulate(n, n)((i, j) => mtx1(i)(j) + mtx2(i)(j))
  }
  //Resta de matrices
  def matrixsubtraction(mtx1: Matriz, mtx2: Matriz): Matriz = {
    val n = mtx1.length
    Vector.tabulate(n, n)((i, j) => mtx1(i)(j) - mtx2(i)(j))
  }
  // Extrallendo Submatrices
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    Vector.tabulate(l, l) { (row, col) => m(i + row)(j + col)
    }
  }
  //Matrcies recursivamente, version secuencial
  def multMatrizRecSec(m1: Matriz, m2: Matriz): Matriz = {
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

      val c11 = matrixAddition(multMatrizRecSec(a, e), multMatrizRecSec(b, g))
      val c12 = matrixAddition(multMatrizRecSec(a, f), multMatrizRecSec(b, h))
      val c21 = matrixAddition(multMatrizRecSec(c, e), multMatrizRecSec(d, g))
      val c22 = matrixAddition(multMatrizRecSec(c, f), multMatrizRecSec(d, h))

      val top = c11.zip(c12).map { case (row1, row2) => row1 ++ row2 }
      val bottom = c21.zip(c22).map { case (row1, row2) => row1 ++ row2 }
      top ++ bottom
    }}
// Algoritmo de Strassen
  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    // Recibe m1 y m2 matrices cuadradas de la misma dimensión (potencia de 2)
    // y devuelve la multiplicación de las dos matrices usando el algoritmo de Strassen


    val n = m1.head.count(_ => true)

    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val m = n / 2

      val a11 = subMatriz(m1, 0, 0, m)
      val a12 = subMatriz(m1, 0, m, m)
      val a21 = subMatriz(m1, m, 0, m)
      val a22 = subMatriz(m1, m, m, m)

      val b11 = subMatriz(m2, 0, 0, m)
      val b12 = subMatriz(m2, 0, m, m)
      val b21 = subMatriz(m2, m, 0, m)
      val b22 = subMatriz(m2, m, m, m)

      val p1 = multStrassen(matrixAddition(a11, a22), matrixAddition(b11, b22))
      val p2 = multStrassen(matrixAddition(a21, a22), b11)
      val p3 = multStrassen(a11, matrixsubtraction(b12, b22))
      val p4 = multStrassen(a22, matrixsubtraction(b21, b11))
      val p5 = multStrassen(matrixAddition(a11, a12), b22)
      val p6 = multStrassen(matrixsubtraction(a21, a11), matrixAddition(b11, b12))
      val p7 = multStrassen(matrixsubtraction(a12, a22), matrixAddition(b21, b22))

      val c11 = matrixsubtraction(matrixAddition(matrixAddition(p1, p4), p7), p5)
      val c12 = matrixAddition(p3, p5)
      val c21 = matrixAddition(p2, p4)
      val c22 = matrixsubtraction(matrixAddition(matrixAddition(p1, p3), p6), p2)

      // Construir la matriz resultante
      Vector.tabulate(n, n) { (i, j) =>
        if (i < m && j < m) c11(i)(j)
        else if (i < m && j >= m) c12(i)(j - m)
        else if (i >= m && j < m) c21(i - m)(j)
        else c22(i - m)(j - m)
      }
    }
  }

  //Multiplicacion Strassen Paralelo
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {

    val n = m1.head.count(_ => true)

    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val m = n / 2
        val a11 = task {
          subMatriz(m1, 0, 0, m)
        }
        val a12 = task {
          subMatriz(m1, 0, m, m)}
        val a21 = task {
          subMatriz(m1, m, 0, m)
        }
        val a22 = task {
          subMatriz(m1, m, m, m)
        }
        val b11 = task {
          subMatriz(m2, 0, 0, m)
        }
        val b12 = task {
          subMatriz(m2, 0, m, m)
        }
        val b21 = task {
          subMatriz(m2, m, 0, m)
        }
        val b22 = task {
          subMatriz(m2, m, m, m)
        }


        val p1 = task {
          multStrassenPar(matrixAddition(a11.join, a22.join), matrixAddition(b11.join, b22.join))
        }
        val p2 = task {
          multStrassenPar(matrixAddition(a21.join, a22.join), b11.join)
        }
        val p3 = task {
          multStrassenPar(a11.join, matrixsubtraction(b12.join, b22.join))
        }
        val p4 = task {
          multStrassenPar(a22.join, matrixsubtraction(b21.join, b11.join))
        }
        val p5 = task {
          multStrassenPar(matrixAddition(a11.join, a12.join), b22.join)
        }
        val p6 = task {
          multStrassenPar(matrixsubtraction(a21.join, a11.join), matrixAddition(b11.join, b12.join))
        }
        val p7 = task {
          multStrassenPar(matrixsubtraction(a12.join, a22.join), matrixAddition(b21.join, b22.join))
        }

        val c11 = matrixsubtraction(matrixAddition(matrixAddition(p1.join, p4.join), p7.join), p5.join)
        val c12 = matrixAddition(p3.join, p5.join)
        val c21 = matrixAddition(p2.join, p4.join)
        val c22 = matrixsubtraction(matrixAddition(matrixAddition(p1.join, p3.join), p6.join), p2.join)



        // Construir la matriz resultante
        Vector.tabulate(n, n) { (i, j) =>
          if (i < m && j < m) c11(i)(j)
          else if (i < m && j >= m) c12(i)(j - m)
          else if (i >= m && j < m) c21(i - m)(j)
          else c22(i - m)(j - m)
        }
      }
    }





}

