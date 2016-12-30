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
                    //
import com.kabouterlabs.matrix.MatrixM
import com.kabouterlabs.matrix.MatrixOperations._
import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaDenseMatrixImplicit
import org.apache.commons.math3.linear.SingularValueDecomposition
import spire.math._
import spire.implicits._
//import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
//import com.kabouterlabs.matrix.implicits.jeigen.JeigenDenseMatrixImplicit._
//import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaDenseMatrixImplicit._
import com.kabouterlabs.matrix.implicits.jblass.JblasDoubleMatrixImplicit._
//import com.kabouterlabs.matrix.implicits.apachecommonsmath.ApacheCommonsMathDenseMatrixImplicit._


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
  val arrL = Array(
    11.3581732686	,
    -3.2020994169	,
    18.0510540869	,
    49.1320555517	,
    -19.6554494037	,
    68.1835081826	,
    0	,
    12.5597197152	,
    -12.4285202038	,
    -49.7426923319	,
    -2.6575993597	,
    43.5535492984	,
    0	,
    0	,
    12.1565345348	,
    55.118878598	,
    23.1703823278	,
    -58.0898301742	,
    0	,
    0	,
    0	,
    17.7743775961	,
    -6.7945561096	,
    77.5340560127	,
    0	,
    0	,
    0	,
    0	,
    36.5250799375	,
    33.354408821	,
    0	,
    0	,
    0	,
    0	,
    0	,
    67.9604918771

  )
  val L = MatrixM(6,6,arrL)
  val arr = Array(2.0,1.0,5.0,7.0,10.56,-90.1, 0.0,6.0,-3.0,2.0,10.0,45.0, 8.0,0.0,7.0,8.0,0.0,100.9,  6.0,1.0,4.0,5.0,-45.0, 34.56 ,0.09,7.0,0.3,0.56,7.0, 0.89,
    5.0, -9.0, 23.0, 90.0 , 5.0, -12.0)
  val matx  = MatrixM(6,6,arr)
  //println(matx)
  val mat   = matx |* matx.transpose
  println(mat.stringefy)
  val res = mat.cholesky
  println(((res.L |* res.L.transpose) :- mat).stringefy)
  println(res.L.stringefy)
  println(L.stringefy)
  println((res.L :- L).stringefy)



}













