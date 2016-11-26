package com.kabouterlabs.matrix.main

/**
  * Created by fons on 3/13/16.
  */


import com.kabouterlabs.matrix.MatrixM
import com.kabouterlabs.matrix.MatrixOperations._
import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaMatImplicit

//import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
//import com.kabouterlabs.matrix.implicits.jeigen.JeigenDenseMatrixImplicit._

import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaMatImplicit._



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
  println(m6)
  val m7 = m6.sumRows()
  println(m7)
  val m8 = m6.sumCols()
  println(m8)

  val a1  = Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0,90.0,33.0,107.0,-78.0,-23.0,14.0,33.0)
  val er1 = Array(81.3567339942231, 64.2735390962677, 7.28804710064899, -61.9183201911397)
  val hsize = math.sqrt(a1.length).toInt
  val l3 = MatrixM(hsize, hsize, a1)
  val e = l3.eig()




  val r = Array[Double](434.00000, 417.00000,  -489.00000,  501.00000,   527.00000,   139.00000,
    959.00000,  1434.00000,  -1668.00000,   1068.00000,   1361.00000,   -506.00000,
    -39.00000,
    -322.00000,
    1047.00000,
    118.00000,
    -2.00000,
    1672.00000)


  val l2 = MatrixM(3,3,r )
  val l3a = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
  println(l2)
  println(l3a)
  val l4 = l2 concatRight l3a
  println(l4)
  val cv = l4(::,0 to 2)
  println(cv)
  val a = (l4(::, 0 to 2) :== l2).sum()
  val b = (l4(::, 3 to 5) :== l3a).sum()

  println("DONE")

}


























