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
import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaDenseMatrixImplicit
import org.apache.commons.math3.linear.SingularValueDecomposition
import spire.math._
import spire.implicits._
//import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
//import com.kabouterlabs.matrix.implicits.jeigen.JeigenDenseMatrixImplicit._
//import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaDenseMatrixImplicit._
//import com.kabouterlabs.matrix.implicits.jblass.JblasDoubleMatrixImplicit._
import com.kabouterlabs.matrix.implicits.apachecommonsmath.ApacheCommonsMathDenseMatrixImplicit._


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
      import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaDenseMatrixImplicit._
      new MatrixExample
    }

   def run4() = {
     import com.kabouterlabs.matrix.implicits.apachecommonsmath.ApacheCommonsMathDenseMatrixImplicit._

     new MatrixExample
   }

  def run5() = {
    import com.kabouterlabs.matrix.implicits.jblass.JblasDoubleMatrixImplicit._

    new MatrixExample
  }


}



object Main extends App
{


  val arr = Array(2.0,1.0,5.0,7.0,0.0, 0.0,6.0,0.0,0.0,10.0, 8.0,0.0,7.0,8.0,0.0,  6.0,1.0,4.0,5.0,0.0 ,0.0,7.0,0.0,0.0,7.0)
  val mat  = MatrixM(5,5,arr)

  val Uarr = Array(-0.54225536	,
  -0.10181247	,
  -0.52495325	,
  -0.64487038	,
  -0.06449519	,
  0.06499573	,
  -0.59346055	,
  0.05938171	,
  0.07040626	,
  -0.79692967	,
  0.82161708	,
  -0.11255162	,
  -0.21296861	,
  -0.50874368	,
  0.09000966	,
  0.10574661	,
  0.78812338	,
  -0.11574223	,
  -0.05990271	,
  -0.59219473	,
  -0.12448979	,
  0.06026999	,
  0.81372354	,
  -0.56282918	,
  -0.04412631	)
  val U = MatrixM(5,5,Uarr)

  val Varr = Array(
    -0.46461713	,
    -0.07008599	,
    -0.73509354	,
    -0.48439167	,
    -0.06496983	,
    0.02150651	,
    -0.75998796	,
    0.09879712	,
    0.0254474	,
    -0.64151954	,
    -0.86850856	,
    0.06307148	,
    0.28400852	,
    0.39886566	,
    -0.04427431	,
    0.00079955	,
    -0.60134567	,
    -0.22348457	,
    0.33268381	,
    0.69120104	,
    -0.17134943	,
    -0.22784122	,
    0.5650402	,
    -0.70352314	,
    0.32328395

  )
  val V = MatrixM(5,5,Varr)
  val Vt = V.transpose

  val Sarr = Array(17.91837086  , 15.17137188   , 3.56400204  ,  1.98422815  ,  0.34955567)
   println(V.stringefy)
  //for ( m <- mat) yield 1
//  println(mat.stringefy)
  val res = mat.svd
  //println(res.U.stringefy)
  println(res.Vt.stringefy)
  println(Vt.stringefy)
  //println(res.S)
  //println(res.Sm)
  //println(U.stringefy)
  //println((res.U :\ U).sum.map((x)=> scala.math.floor(x+0.5) % 5) )
  println((res.Vt :\ Vt).sum.map((x)=> scala.math.floor(x+0.5) % 5) )
  val test = scala.math.floor(0.5 + ((res.Vt :\ Vt).toArray).get.map(scala.math.abs).sum)
  println(test)
  println(res.S.map(_.zip(Sarr).map((x)=> x._1 - x._2).sum).map(scala.math.abs).map((x)=>scala.math.floor(x+0.5)))

  println(((res.Vt |* res.Vt.transpose) :- MatrixM.eye(5)).sum)

  println(res.S.map(_.mkString(",")))
}













