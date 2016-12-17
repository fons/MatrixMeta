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

import com.kabouterlabs.matrix.MatrixOperations._


/**
  * Created by fons on 12/2/16.
  */

case class MatrixExample[T](implicit evmatrix: MatrixOperationsTC[T]) {

  def apply(): Unit = {
    val m1a = matrix(3, 3, Array(5, 6, 7, 8, 9, 8, 7, 6, 5))
    val m56 = matrix(3,3)
    val m2a = fill(3, 3, 78.23)
    val m3a = m1a |* m2a :* m2a
    println(m1a)
    println(m2a)
    println(m3a)



    val ar = Array(4.0, 90.0, 6.0, 7.0, 2.0, 3.0, 4.0, 5.0, 6.0, -34.0, -89.0, -12.78)

    val m1 = zero(5, 5)
    val m2 = matrix(2, 6, ar)

    println(m1)
    println(m2)
    val m3 = fill(10, 20, -88.89)
    println(m3)
    val m4 = one(10, 20)
    println(m4)
    val m6 = m3 :+ m3

    println(m6)
    val m7 = add(m3, m3)
    val m7a = m6.sumRows
    println(m7a)

    val m8 = m6.sumCols
    println(m8)

    val a1 = Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0, 90.0, 33.0, 107.0, -78.0, -23.0, 14.0, 33.0)
    val er1 = Array(81.3567339942231, 64.2735390962677, 7.28804710064899, -61.9183201911397)
    val hsize = math.sqrt(a1.length).toInt
    val l3 = matrix(hsize, hsize, a1)
    val ey = eigen(l3)
    println(ey)




    val r = Array[Double](434.00000, 417.00000, -489.00000, 501.00000, 527.00000, 139.00000,
      959.00000, 1434.00000, -1668.00000, 1068.00000, 1361.00000, -506.00000,
      -39.00000,
      -322.00000,
      1047.00000,
      118.00000,
      -2.00000,
      1672.00000)


    val l2 = matrix(3, 3, r)
    val l3a = matrix(3, 3, Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0))
    println(l2)
    println(l3a)
    val l4 = l2 concatRight l3a
    println(l4)
    val l4i = l2.inverse
    inverse(l2) :== l4i
    val l5 = l2 concatDown l3a
    println(l5)
    val cv = l4(::, 0 to 2)
    println(cv)
    l4.deepCopy :== deepCopy(l4)
    slice(setValue(l4.deepCopy,1,1,67.90),::,0 to 3)
    val a = (l4(::, 0 to 2) :== l2).sum
    val b = (l4(::, 3 to 5) :== l3a).sum
    val l6a = l4(::, 3 to 5)
    println(l6a)
    println(evmatrix)

    val s1  = l2 :+ l3a
    val s1b = add(l2, l3a)
    val s2  = l2 :- l3a
    val s2b = subtract(l2, l3a)
    val s3  = l2 :\ l3a
    val s3b = divide(l2, l3a)
    val s4  = l2 :* l3a
    val s4b = hadamard(l2, l3a)
    val s5  = l2 |* l3a
    val s5a = multiply(l2, l3a)

    val s1a  = l2 ++ 7.0
    val s1aa = divide1(l2, 7.0)
    val s2a  = l2 -- 7.0
    val s2aa = subtract1(l2, 7.0)
    val s3a  = l2 ** 7.0
    val s3aa = multiply1(l2, 7.0)
    val s4a  = l2 \\ 7.0
    val s4aa = divide1(l2, 7.0)

    val ta1  = s1 :== s1a
    val ta1a = mEqual(s1,s1a)
    val ta2 = s1 :<= s1a
    val ta3 = s1 :<<  s1a
    val ta4 = s1 :>>  s1a
    val ta5 = s1 :>=  s1a
    val ta6 = s1 :!=  s1a
    println(l2,l3a)

    val si  = s1.inverse
    val sia = inverse(s1)
    val sd  = s1.determinant
    val sda = determinant(s1)
    val st  = s1.transpose
    val sta = transpose(s1)
    val tr  = s1.trace
    val tra = trace(s1)
    val eig = s1.eigen
    val eia = eigen(s1)
    val rs1 = s1.solve(s2)
    val rs1a =solve(s1,s2)

    val k  = rs1a.sumCols.sumRows
    val ka = sumCols(sumRows(rs1a))
    ka.csvWrite("nn")
    csvRead("nn")

  }

}

