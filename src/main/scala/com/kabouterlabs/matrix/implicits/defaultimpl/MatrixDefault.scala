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

package com.kabouterlabs.matrix.implicits.defaultimpl

import com.kabouterlabs.matrix.MatrixOperations.MatrixOperationsTC
import com.kabouterlabs.matrix.{MatrixM, CompanionT, FactoryT}
import spire.math.Numeric

/**
  * Created by fons on 5/9/16.
  */

object MatrixDefaultImplicit {
  type ElemT = Double
  type MatrixImpl = {}
  type MatrixDouble = MatrixM[MatrixImpl]

  implicit val fdouble$ = new FactoryT {
    type MatrixImplT = MatrixImpl

    implicit override def apply(row: Int, col: Int, data: Array[Double]): Option[MatrixImplT] = None

    implicit override def apply(row: Int, col: Int): Option[MatrixImplT] = None
  }

  implicit object MatrixOperationsTC$implicit$ extends MatrixOperationsTC[MatrixDouble] {

    override def deepCopy(lhs: MatrixDouble): MatrixDouble = MatrixM.none

    override type EigenResultT = Option[Double]

    override def eig(m: MatrixDouble): Option[Double] = None

    override def vectors(r: EigenResultT): MatrixDouble = MatrixM.none

    override def values(r: EigenResultT): MatrixDouble = MatrixM.none

    override def eigen(m: MatrixDouble): (MatrixDouble, MatrixDouble) = (MatrixM.none, MatrixM.none)

    override def add(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = MatrixM.none

    override def sub(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = MatrixM.none

    override def mult(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = MatrixM.none

    override def multe(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = MatrixM.none

    override def dive(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = MatrixM.none

    override def add1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble = MatrixM.none

    override def sub1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble = MatrixM.none

    override def mul1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble = MatrixM.none

    override def div1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble = MatrixM.none

    override def eq(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = MatrixM.none

    override def ne(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = MatrixM.none

    override def gt(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = MatrixM.none

    override def ge(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = MatrixM.none

    override def lt(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = MatrixM.none

    override def le(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = MatrixM.none

    override def create(rows: Int, colls: Int, data: Array[ElemT]): MatrixDouble = MatrixM.none

    override def zero(row: Int, col: Int): MatrixDouble = MatrixM.none

    override def rand(row: Int, col: Int): MatrixDouble = MatrixM.none

    override def eye(size: Int): MatrixDouble = MatrixM.none

    override def diag(data: Array[ElemT]): MatrixDouble = MatrixM.none

    override def one(row: Int, col: Int): MatrixDouble = MatrixM.none

    override def fill(row: Int, col: Int, value: ElemT): MatrixDouble = MatrixM.none

    override def inverse(m: MatrixDouble): MatrixDouble = MatrixM.none

    override def solve(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = MatrixM.none

    override def transpose(m: MatrixDouble): MatrixDouble = MatrixM.none

    override def create(rows: Int, colls: Int): MatrixDouble = MatrixM.none

    override def determinant(m: MatrixDouble): Option[ElemT] = None

    override def get(m: MatrixDouble, row: Int, coll: Int): Option[ElemT] = None

    override def concatDown(m: MatrixDouble, down: MatrixDouble): MatrixDouble = MatrixM.none

    override def set(m: MatrixDouble, row: Int, coll: Int, v: ElemT): MatrixDouble = MatrixM.none

    override def toArray(m: MatrixDouble): Option[Array[ElemT]] = None

    override def concatRight(m: MatrixDouble, rhs: MatrixDouble): MatrixDouble = MatrixM.none

    override def toDiag(m: MatrixDouble): MatrixDouble = MatrixM.none

    override def slice[K, L](m: MatrixDouble, row: K, col: L): MatrixDouble = MatrixM.none

    override def csvWrite(fn: String, u: MatrixDouble): Unit = Unit

    override def csvRead(fn: String): MatrixDouble = MatrixM.none

    override def sumRows(m: MatrixDouble): MatrixDouble = MatrixM.none

    override def sumCols(m: MatrixDouble): MatrixDouble = MatrixM.none

    override def sum(m: MatrixDouble): Option[ElemT] = None

    override def trace(m: MatrixDouble): Option[ElemT] = None

    override def none  = MatrixM.none

  }

  implicit object JeigenDenseMatrixCompanion extends CompanionT {
    override type MatrixImplT = MatrixDouble

    override def fill(row: Int, col: Int, value: ElemT): MatrixDouble = MatrixM.none

    override def ones(row: Int, col: Int): MatrixDouble = MatrixM.none

    override def rand(row: Int, col: Int): MatrixImplT = MatrixM.none

    override def eye(size: Int): MatrixImplT = MatrixM.none

    override def zeros(row: Int, col: Int): MatrixImplT = MatrixM.none

    //TODO check this
    override def diag(data: Array[Double]): MatrixImplT = MatrixM.none
  }

}