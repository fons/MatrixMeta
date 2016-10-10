package com.kabouterlabs.matrix.main

/**
  * Created by fons on 3/13/16.
  */


import com.kabouterlabs.matrix.MatrixM
import com.kabouterlabs.matrix.MatrixOperations._
import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._

object Main extends App
{

  val ar = Array(4.0,90.0,6.0 ,7.0,2.0,3.0,4.0,5.0,6.0,-34.0,-89.0,-12.78)

  val m1 = MatrixM(5,5)
  val m2 = MatrixM(2,6, ar)

  println(m1)
  println(m2)
  val m3 = MatrixM.fill(10,20, -88.89)
  println(m3)
  val m4 = MatrixM.ones(10,20)
  println(m4)
  val m6 = m3 :+ m3

  println("DONE")

}


























