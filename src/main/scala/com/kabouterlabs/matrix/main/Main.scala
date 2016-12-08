/*
 * Copyright (c) 2016.
 * https://opensource.org/licenses/BSD-3-Clause
 *
 * Copyright (c) 2016, MatrixMeta developers
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list
 *    of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO,THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 * THE COPYRIGHT HOLDER OR CONTRIBUTORS BELIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * EOM
 */

package com.kabouterlabs.matrix.main

import com.kabouterlabs.matrix.MatrixM
import com.kabouterlabs.matrix.MatrixOperations._
import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._

/**
  * Created by fons on 3/13/16.
  */



object Use
{
    def run1() = {
      import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
      new MatrixExample
    }

    def run2() = {
      import com.kabouterlabs.matrix.implicits.jeigen.JeigenDenseMatrixImplicit._
      new MatrixExample
    }

    def run3() = {
      import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaMatImplicit._
      new MatrixExample
    }


}

case class TestIt[A](n:A){
  def map[B](f:A=>B):B = f(n)
  def flatMap[B](f:A=>TestIt[B]):TestIt[B] = f(n)

}
object Main extends App {


  val mat1 = MatrixM(2,2)

  Use.run1()()
  Use.run2()()
  val t = Use.run3()
  t()
  println(MatrixM.one(5,4))
  println(MatrixM.rand(5,4))
  println(MatrixM.none)
  val n1 = Option(23.0)
  val n2 = Option(45.6)
  val c = for (a <- n1; b <- n2) yield {
    a*b
  }
  println(n1,n2,c)
  println("DONE")
  val m1 = new TestIt(78)
  val m2 = new TestIt(89)
  for (a <- m1; b <- m2) yield {
    //println(a,b3)
    new TestIt(a*b)
  }

  val mat2 = MatrixM.one(2,2)
  val mat3 = MatrixM.rand(2,4)
  println(mat2)
  val r000 = Array[Double](434.00000, 417.00000,  -489.00000,  501.00000,   527.00000,   139.00000,
    959.00000,  1434.00000,  -1668.00000,   1068.00000,   1361.00000,   -506.00000,
    -39.00000, -322.00000, 1047.00000, 118.00000, -2.00000, 1672.00000)
  val t000 = Array[Double](434.00000 + 501.00000 + 959.00000,
    417.00000 + 527.00000 + 1434.00000,
    -489.00000 + 139.00000 -1668.00000)


    val a = MatrixM(3, 3, r000).sumCols()
    val b = MatrixM(3, 1, t000)
    val c0 = (a :== b)//.sum()

    println(a,b,c0)

  for (x1 <- a; x2 <-b) yield {
      a :== b
  }
  //mat3 |* mat2

}


























