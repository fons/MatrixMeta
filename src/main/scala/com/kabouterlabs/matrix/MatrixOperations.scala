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
package com.kabouterlabs.matrix


/**
  * Created by fons on 3/20/16.
  */

import spire.math.Numeric


object MatrixOperations {


  trait MatrixOperationsTC[A] {
    type MatrixDataElemT  // = Double

    type EigenResultRetTypeT

    def rows(m:A):Int

    def columns(m:A):Int

    def isNull(m:A):Boolean

    def size(m:A):Int

    def add(x: A, y: A): A

    def sub(x: A, y: A): A

    def mult(lhs: A, rhs: A): A

    def multe(lhs: A, rhs: A): A

    def dive(lhs: A, rhs: A): A

    def add1[B: Numeric](x: A, y: B): A

    def sub1[B: Numeric](x: A, y: B): A

    def mul1[B: Numeric](x: A, y: B): A

    def div1[B: Numeric](x: A, y: B): A

    def eq(lhs: A, rhs: A): A

    def ne(lhs: A, rhs: A): A

    def ge(lhs: A, rhs: A): A

    def le(lhs: A, rhs: A): A

    def gt(lhs: A, rhs: A): A

    def lt(lhs: A, rhs: A): A

    def create(rows: Int, colls: Int, data: Array[MatrixDataElemT]): A

    def create(rows: Int, colls: Int): A

    def zero(row: Int, col: Int): A

    def eye(size: Int): A

    def rand(row: Int, col: Int): A

    def diag(data: Array[MatrixDataElemT]): A

    def one(row: Int, col: Int): A

    def none: Unit

    def fill(row: Int, col: Int, value: MatrixDataElemT): A

    def inverse(m: A): A

    def transpose(m: A): A

    def determinant(m: A): Option[MatrixDataElemT]

    def solve(lhs: A, rhs: A): A

    def deepCopy(lhs:A):A

    def get(m: A, row: Int, coll: Int): Option[MatrixDataElemT]

    def set(m: A, row: Int, coll: Int, v: MatrixDataElemT): A

    def slice[K, L](m: A, row: K, col: L): A

    def toArray(m: A): Option[Array[MatrixDataElemT]]

    def concatRight(m: A, rhs: A): A

    def concatDown(m: A, down: A): A

    def toDiag(m: A): A

    def csvWrite(fn: String, u: A): Unit

    def csvRead(fn: String): A

    def sumRows(m: A): A

    def sumCols(m: A): A

    def trace(m: A): Option[MatrixDataElemT]

    def sum(m: A): Option[MatrixDataElemT]

    def eig(m: A): EigenResultRetTypeT //EigenResultM[EigenResultT]
    def vectors(r: EigenResultRetTypeT): Option[EigenAccessT[EigenResultRetTypeT]#EigenVectorT]

    def values(r: EigenResultRetTypeT): Option[EigenAccessT[EigenResultRetTypeT]#EigenValuesT]

    def eigen(m:A): (Option[EigenAccessT[EigenResultRetTypeT]#EigenValuesT], Option[EigenAccessT[EigenResultRetTypeT]#EigenVectorT])

    def svd(m:A) : SingularValueDecompositionT[A]#SvdResultT

    def qr(m:A) : QRDecompositionT[A]#QRResultT

    def lu(m:A) : LUDecompositionT[A]#LUResultT

    def cholesky(m:A): CholeskyDecompositionT[A]#CholeskyResultT

  }


  //////////////////////////////////////////////////////////////////////

  implicit class MatrixOperator$[A](lhs: A)(implicit ev: MatrixOperationsTC[A]) {
    def :+(rhs: A) = ev.add(lhs, rhs)

    def :-(rhs: A) = ev.sub(lhs, rhs)

    def |*(rhs: A) = ev.mult(lhs, rhs)

    def :*(rhs: A) = ev.multe(lhs, rhs)

    def :\(rhs: A) = ev.dive(lhs, rhs)

    def ++[B: Numeric](rhs: B) = ev.add1(lhs, rhs)

    def --[B: Numeric](rhs: B) = ev.sub1(lhs, rhs)

    def **[B: Numeric](rhs: B) = ev.mul1(lhs, rhs)

    def \\[B: Numeric](rhs: B) = ev.div1(lhs, rhs)

    def :==(rhs: A): A = ev.eq(lhs, rhs)

    def :!=(rhs: A): A = ev.ne(lhs, rhs)

    def :>=(rhs: A): A = ev.ge(lhs, rhs)

    def :<=(rhs: A): A = ev.le(lhs, rhs)

    def :>>(rhs: A): A = ev.gt(lhs, rhs)

    def :<<(rhs: A): A = ev.lt(lhs, rhs)

    def concatRight(rhs: A): A = ev.concatRight(lhs, rhs)

    def concatDown(rhs: A): A = ev.concatDown(lhs, rhs)

    def solve(rhs: A): A = ev.solve(lhs, rhs)
  }

  implicit class MatrixSelfOps$[A](lhs: A)(implicit ev: MatrixOperationsTC[A]) {

    type MatrixElemT =  MatrixOperationsTC[A]#MatrixDataElemT

    def inverse: A = ev.inverse(lhs)

    def transpose: A = ev.transpose(lhs)

    def determinant:Option[MatrixElemT] = ev.determinant(lhs)

    def deepCopy = ev.deepCopy(lhs)

    def apply[K, L](row: K, col: L): A = ev.slice(lhs, row, col)

    def apply(row: Int, coll: Int): Option[MatrixElemT] = ev.get(lhs, row, coll)

    def apply(row: Int, coll: Int, v: ev.MatrixDataElemT): A = ev.set(lhs, row, coll, v)

    def toDiag: A = ev.toDiag(lhs)

    def csvWrite(fn: String): Unit = ev.csvWrite(fn, lhs)

    def sumRows = ev.sumRows(lhs)

    def sumCols = ev.sumCols(lhs)

    def trace:Option[MatrixElemT] = ev.trace(lhs)

    def sum:Option[MatrixElemT] = ev.sum(lhs)

    def eigen  = {val e = ev.eig(lhs); (ev.values(e), ev.vectors(e))}

    def svd : SingularValueDecompositionT[A]#SvdResultT  = ev.svd(lhs)

    def qr : QRDecompositionT[A]#QRResultT = ev.qr(lhs)

    def lu : LUDecompositionT[A]#LUResultT = ev.lu(lhs)

    def cholesky : CholeskyDecompositionT[A]#CholeskyResultT = ev.cholesky(lhs)

  }


  def add[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :+ rhs

  def subtract[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :- rhs

  def multiply[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs |* rhs

  def hadamard[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :* rhs

  def schur[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :* rhs

  def divide[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :\ rhs

  def add1[A: MatrixOperationsTC, B: Numeric](lhs: A, rhs: B) = lhs ++ rhs

  def subtract1[A: MatrixOperationsTC, B: Numeric](lhs: A, rhs: B) = lhs -- rhs

  def multiply1[A: MatrixOperationsTC, B: Numeric](lhs: A, rhs: B) = lhs ** rhs

  def divide1[A: MatrixOperationsTC, B: Numeric](lhs: A, rhs: B) = lhs \\ rhs

  def mEqual[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :== rhs

  def mNotEqual[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :!= rhs

  def mGreaterEqual[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :>= rhs

  def mSmallerEqual[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :<= rhs

  def mGreater[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :>> rhs

  def mSmaller[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :<< rhs

  def toDiag[A : MatrixOperationsTC](lhs:A):A = lhs.toDiag

  def concatRight[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs concatRight rhs

  def concatDown[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs concatDown rhs

  def solve[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs solve rhs

  def inverse[A: MatrixOperationsTC](lhs: A): A = lhs.inverse

  def transpose[A: MatrixOperationsTC](lhs: A): A = lhs.transpose

  def trace[A: MatrixOperationsTC](lhs: A) = lhs.trace

  def determinant[A: MatrixOperationsTC](lhs: A) = lhs.determinant

  def slice[A: MatrixOperationsTC, K, L](lhs: A, row: K, col: L): A = lhs(row, col)

  def deepCopy[A: MatrixOperationsTC](lhs: A): A = lhs.deepCopy

  def getValue[A: MatrixOperationsTC](lhs: A, row: Int, coll: Int)(implicit ev: MatrixOperationsTC[A]) = ev.get(lhs,row, coll)

  def setValue[A](lhs: A, row: Int, coll: Int, v: Double)(implicit ev: MatrixOperationsTC[A]{type MatrixDataElemT=Double}) = ev.set(lhs, row, coll, v)

  def matrix[A](row: Int, col: Int, data: Array[Double])(implicit ev: MatrixOperationsTC[A]{type MatrixDataElemT=Double}) = ev.create(row, col, data)

  def matrix[A](row :Int, col :Int)(implicit ev: MatrixOperationsTC[A]) = ev.create(row,col)

  def zero[A](row: Int, col: Int)(implicit ev: MatrixOperationsTC[A]) = ev.zero(row, col)

  def eye[A](size: Int)(implicit ev: MatrixOperationsTC[A]): A = ev.eye(size)

  def rand[A](row: Int, col: Int)(implicit ev: MatrixOperationsTC[A]): A = ev.rand(row, col)

  def diag[A](data: Array[Double])(implicit ev: MatrixOperationsTC[A]{type MatrixDataElemT=Double}): A = ev.diag(data)

  def one[A](row: Int, col: Int)(implicit ev: MatrixOperationsTC[A]): A = ev.one(row, col)

  def none[A]()(implicit ev: MatrixOperationsTC[A])  = ev.none

  def fill[A](row: Int, col: Int, value:Double)(implicit ev: MatrixOperationsTC[A]{type MatrixDataElemT=Double}) = ev.fill(row, col, value)

  def fill[A](row: Int, col:Int)(implicit ev: MatrixOperationsTC[A]) = (value:ev.MatrixDataElemT)=>ev.fill(row, col, value)

  def csvRead[A](fn: String)(implicit ev: MatrixOperationsTC[A]): A = ev.csvRead(fn)

  def csvWrite[A](fn:String, lhs:A)(implicit ev: MatrixOperationsTC[A]):Unit = ev.csvWrite(fn, lhs)

  def eigen[A](lhs: A)(implicit ev: MatrixOperationsTC[A]) = {val e = ev.eig(lhs); (ev.values(e), ev.vectors(e))}

  def sumRows[A](lhs :A)(implicit ev: MatrixOperationsTC[A]):A = lhs.sumRows

  def sumCols[A](lhs :A)(implicit ev: MatrixOperationsTC[A]):A = lhs.sumCols

  def svd[A](lhs: A)(implicit ev: MatrixOperationsTC[A]) = ev.svd(lhs)

  def qr[A](lhs:A)(implicit ev: MatrixOperationsTC[A]) = ev.qr(lhs)

  def lu[A](lhs:A)(implicit ev: MatrixOperationsTC[A]) = ev.lu(lhs)

  def cholesky[A](lhs:A)(implicit ev: MatrixOperationsTC[A]) = ev.cholesky(lhs)

}
