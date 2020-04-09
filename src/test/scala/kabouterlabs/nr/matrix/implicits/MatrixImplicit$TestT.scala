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

package test.kabouterlabs.nr.matrix.implicits

import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
import com.kabouterlabs.matrix.MatrixM
import com.kabouterlabs.matrix.MatrixOperations._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


/**
  * Created by fons on 4/19/16.
  */
trait MatrixImplicit$TestT[U] extends AnyFlatSpec with Matchers {
  //this =>


  val t = Array[Double]( 4.0, 5.0, 6.0, 7.0, 8.0,21.0,
    56.0,-1.0,-9.0,67.0,45.0,89.0,
    23.0,67.0,-78.0,23.0,45.0,-65.0,
    90.0,89.0)

  val rows  = 6
  val colls = 3
    def throwAssert () {
      assert(1==2, "ok get a clue")
    }

  "a matrix" should "be created" in {
    MatrixM(rows,colls,t)  should not equal None
  }

//  it should "the infix operator for element wise add" in {
//    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
//    val l2 = MatrixM(rows,colls,t)
//    val l0 = add(l3,l2)(matrixOps)
//
//  }
//
//  it should "the infix operator for element wise multiply" in {
//    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
//    val l2 = MatrixM(rows,colls,t)
//    val l0 = multe(l3,l2)
//
//  }
//
//  it should "the infix operator for matrix multiply" in {
//    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
//    val l2 = MatrixM(rows,colls,t)
//    val l0 = mult(l3,l2)
//
//  }

  it should "the infix operator for element wise add" in {
    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
    val l2 = MatrixM(rows,colls,t)
    val l0 = add(l3,l2)

  }

  it should "the infix operator for element wise subtract" in {
    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
    val l2 = MatrixM(rows,colls,t)
    val l0 = subtract(l3,l2)
  }

  it should "use the infix operator for element wise divide" in {
    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
    val l2 = MatrixM(rows,colls,t)
    val l0 = divide(l3,l2)

  }

}
