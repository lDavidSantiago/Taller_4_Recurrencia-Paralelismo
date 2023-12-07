/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package taller_4_paralelismo_y_recursion
import org.scalameter.withWarmer
import org.scalameter.Warmer.Default

import scala.collection.parallel.CollectionConverters._
import org.scalameter.measurer

import scala.collection.parallel.immutable.ParVector


/**
 * Taller 4: Paralelismo y recursión
 * Authors: David Santiago Velasco Triana, Jhojan Stiven Castaño Jejenm, William Alexander Franco Otero
 * Profesor: Carlos A Delgado
 */
object App {
  type Matriz = Vector[Vector[Int]]
  def main(args: Array[String]): Unit = {
    val matrices = new Matrices()
   //val matrizA = matrices.matrizAlAzar(2,2)
    //val matrizB = matrices.matrizAlAzar(2,2)
    val vectorA = matrices.vectorAlAzar(100000,1000000)
    val vectorB = matrices.vectorAlAzar(1000000,1000000)
    val ve1 = (vectorA).par
    val ve2 = (vectorB).par
    def compararAlgoritmos(f1: (Matriz, Matriz) => Matriz, f2: (Matriz, Matriz) => Matriz, m1: Matriz, m2: Matriz): Unit = {
      val time1 = withWarmer(new Default) measure {
        f1(m1, m2)
      }
      println(s"Tiempo de ejecucion de la funcion reqsec: $time1")
      val time2 = withWarmer(new Default) measure {
        f2(m1, m2)
      }
      println(s"Tiempo de ejecucion de la funcion par: $time2")

      val aceleracion = time1.value / time2.value
      println(s"aceleracion: $aceleracion")
    }
    //compararAlgoritmos(matrices.multMatrizRecSec, matrices.multMatrizRecSec, matrizA, matrizB)
    def compararProdPunto(f1: (Vector[Int], Vector[Int]) => Int, f2: (ParVector[Int], ParVector[Int]) => Int, m1: Vector[Int], m2: Vector[Int],v1 : ParVector[Int],v2: ParVector[Int]): Unit = {
      val time1 = withWarmer(new Default) measure {
        f1(m1, m2)
      }

      println(s"Tiempo de ejecucion de la funcion reqsec: $time1")
      val time2 = withWarmer(new Default) measure {
        f2(ve1, ve2)
      }
      println(s"Tiempo de ejecucion de la funcion par: $time2")

      val aceleracion = time1.value / time2.value
      println(s"aceleracion: $aceleracion")
    }
    compararProdPunto(matrices.prodPunto,matrices.prodPuntoParD, vectorA, vectorB,ve1,ve2)


  }

}
