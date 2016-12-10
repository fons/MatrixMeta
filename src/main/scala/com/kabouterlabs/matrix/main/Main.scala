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
//import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
import com.kabouterlabs.matrix.implicits.jeigen.JeigenDenseMatrixImplicit._
//import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaMatImplicit._


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

object EigenTest {


  def run1() = {
    import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
    val mm1= matrix(3, 3, Array(-3.0, 1.0, -2.0, 0.0, -1.0, -1.0, 2.0, 0.0, 0.0))

    val eigr = eigen(mm1)
    println(mm1)
    println("(complex) eigen values : " , eigr._1, "eigen vectors :", eigr._2)
  }
  def run2() = {
    import com.kabouterlabs.matrix.implicits.jeigen.JeigenDenseMatrixImplicit._

    val mm1= matrix(3, 3, Array(-3.0, 1.0, -2.0, 0.0, -1.0, -1.0, 2.0, 0.0, 0.0))

    val eigr = eigen(mm1)
    println(mm1)
    println("(complex) eigen values : " , eigr._1, "eigen vectors :", eigr._2)
  }
}
object Main extends App {



//  Use.run1()()
//  Use.run2()()
//  val t = Use.run3()
//  t()


//  println(MatrixM.one(5,4))
//  println(MatrixM.rand(5,4))
//  println(MatrixM.none)
//  val n1 = Option(23.0)
//  val n2 = Option(45.6)
//  val c = for (a <- n1; b <- n2) yield {
//    a*b
//  }
//  println(n1,n2,c)
//  println("DONE")
//  val m1 = new TestIt(78)
//  val m2 = new TestIt(89)
//  for (a <- m1; b <- m2) yield {
//    //println(a,b3)
//    new TestIt(a*b)
//  }
//
//  val mat1 = MatrixM(2,2)
//  val mat2 = MatrixM.one(2,2)
//  val mat3 = MatrixM.rand(2,4)
//
//  val r000 = Array[Double](434.00000, 417.00000,  -489.00000,  501.00000,   527.00000,   139.00000,
//    959.00000,  1434.00000,  -1668.00000,   1068.00000,   1361.00000,   -506.00000,
//    -39.00000, -322.00000, 1047.00000, 118.00000, -2.00000, 1672.00000)
//  val mat4 = MatrixM(3, 3, r000)
//  println(mat1,mat2,mat3,mat4)

  //  val t000 = Array[Double](434.00000 + 501.00000 + 959.00000,
//    417.00000 + 527.00000 + 1434.00000,
//    -489.00000 + 139.00000 -1668.00000)
//
//
//    val a = MatrixM(3, 3, r000).sumCols
//    val b = MatrixM(3, 1, t000)
//    val c0 = (a :== b)//.sum()
//
//    println(a,b,c0)
//
//  for (x1 <- a; x2 <-b) yield {
//
//  }
//  mat3 |* mat2
//
  val mm = MatrixM.rand(10,10)
  val mc = mm.deepCopy(2,1,89.90)
  deepCopy(mc)
//
//  val s1 = mm(2,1)
//  val s2 = mc(2,1)
//
//  println(s1,s2, mm, mm(::,2))
//  val mmd = MatrixM.rand(10,10)
//  val ss = mmd(::,0 to 3)
//  ss(2,1,909.890)
//  println(mmd, ss)

  val l2 = rand(3, 3)
  val l3 = matrix(3, 3, Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0))
//  val s1  = l2 :+ l3
//  val s11 = add(l2,l3)
//  val s2 = l2 :- l3
//  val s22 = subtract(l2,l3)
//  val s3 = l2 :\ l3
//  val s4 = l2 :* l3
//  val s5 = l2 |* l3
//
//  val s1a = l2 ++ 7.0
//  val s2a = l2 -- 7.0
//  val s3a = l2 ** 7.0
//  val s4a = l2 \\ 7.0
//
//  val ta1 = s1 :== s1a
//  val ta2 = s1 :<= s1a
//  val ta3 = s1 :<<  s1a
//  val ta4 = s1 :>>  s1a
//  val ta5 = s1 :>=  s1a
//  val ta6 = s1 :!=  s1a
//  println(l2,l3)
//  println("------------")
//  println(s1,s5,s1a,s3a,ta5)
//
//  val l5 = l2 concatDown l3
//  l5(0,0,-90000)
//  println(l2(0,0),l5(0,0))
//  println(l2.toDiag)

//  val mm0 = rand(3, 3)
//  val mm1= matrix(3, 3, Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0))
//  println("mm0", mm0, "mm1", mm1)
//  println("mm1.inverse", mm1.inverse,"mm1.transpose", mm1.transpose, "mm1.determinant" ,mm1.determinant, mm0.trace)
//  val res = mm1.solve(mm0)
//  println("solving mm1 * res = mm0; res = ", res, "residual : ",(mm1 |* res) :- mm0 )
//  val eigr = eigen(mm1)
//
//  println("(complex) eigen values : " , eigr._1, "eigen vectors :", eigr._2)
//
//  val eigm = mm0.eig
  EigenTest.run1()
  EigenTest.run2()

}


























