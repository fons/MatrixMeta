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
    type EigenResultT

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

    def create(rows: Int, colls: Int, data: Array[Double]): A

    def create(rows: Int, colls: Int): A

    def zero(row: Int, col: Int): A

    def eye(size: Int): A

    def rand(row: Int, col: Int): A

    def diag(data: Array[Double]): A

    def one(row: Int, col: Int): A

    def none

    def fill(row: Int, col: Int, value: Double): A

    def inverse(m: A): A

    def transpose(m: A): A

    def determinant(m: A): Option[Double]

    def solve(lhs: A, rhs: A): A

    def get(m: A, row: Int, coll: Int): Option[Double]

    def set(m: A, row: Int, coll: Int, v: Double): A

    def slice[K, L](m: A, row: K, col: L): A

    def toArray(m: A): Option[Array[Double]]

    def concatRight(m: A, rhs: A): A

    def concatDown(m: A, down: A): A

    def toDiag(m: A): A

    def csvWrite(fn: String, u: A): Unit

    def csvRead(fn: String): A

    def sumRows(m: A): A

    def sumCols(m: A): A

    def trace(m: A): Option[Double]

    def sum(m: A): Option[Double]

    def eig(m: A): EigenResultT

    def vectors(r: EigenResultT): A

    def values(r: EigenResultT): A
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

    def inverse: A = ev.inverse(lhs)

    def transpose: A = ev.transpose(lhs)

    def determinant: Option[Double] = ev.determinant(lhs)

    def apply[K, L](row: K, col: L): A = ev.slice(lhs, row, col)

    def apply(row: Int, coll: Int): Option[Double] = ev.get(lhs, row, coll)

    def apply(row: Int, coll: Int, v: Double): A = ev.set(lhs, row, coll, v)

    def toDiag: A = ev.toDiag(lhs)

    def csvWrite(fn: String): Unit = ev.csvWrite(fn, lhs)

    def sumRows = ev.sumRows(lhs)

    def sumCols = ev.sumCols(lhs)

    def trace = ev.trace(lhs)

    def sum = ev.sum(lhs)

  }


  def add[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :+ rhs

  def sub[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :- rhs

  def mulm[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs |* rhs

  def mul[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :* rhs

  def div[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :\ rhs

  def add1[A: MatrixOperationsTC, B: Numeric](lhs: A, rhs: B) = lhs ++ rhs

  def sub1[A: MatrixOperationsTC, B: Numeric](lhs: A, rhs: B) = lhs -- rhs

  def mul1[A: MatrixOperationsTC, B: Numeric](lhs: A, rhs: B) = lhs ** rhs

  def div1[A: MatrixOperationsTC, B: Numeric](lhs: A, rhs: B) = lhs \\ rhs

  def meq[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :== rhs

  def mne[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :!= rhs

  def mge[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :>= rhs

  def mle[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :<= rhs

  def mgt[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :>> rhs

  def mlt[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs :<< rhs

  def concatRight[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs concatRight rhs

  def concatDown[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs concatDown rhs

  def solve[A: MatrixOperationsTC](lhs: A, rhs: A) = lhs solve rhs

  def inverse[A: MatrixOperationsTC](lhs: A): A = lhs.inverse

  def slice[A: MatrixOperationsTC, K, L](lhs: A, row: K, col: L): A = lhs(row, col)

  def getValue[A: MatrixOperationsTC](lhs: A, row: Int, coll: Int): Option[Double] = lhs(row, coll)

  def setValue[A: MatrixOperationsTC](lhs: A, row: Int, coll: Int, v: Double): A = lhs(row, coll, v)

  def matrix[A](row: Int, col: Int, data: Array[Double])(implicit ev: MatrixOperationsTC[A]) = ev.create(row, col, data)

  def matrix[A](row :Int, col :Int)(implicit ev: MatrixOperationsTC[A]) = ev.create(row,col)

  def zero[A](row: Int, col: Int)(implicit ev: MatrixOperationsTC[A]) = ev.zero(row, col)

  def eye[A](size: Int)(implicit ev: MatrixOperationsTC[A]): A = ev.eye(size)

  def rand[A](row: Int, col: Int)(implicit ev: MatrixOperationsTC[A]): A = ev.rand(row, col)

  def diag[A](data: Array[Double])(implicit ev: MatrixOperationsTC[A]): A = ev.diag(data)

  def one[A](row: Int, col: Int)(implicit ev: MatrixOperationsTC[A]): A = ev.one(row, col)

  def none[A]()(implicit ev: MatrixOperationsTC[A])  = ev.none

  def fill[A](row: Int, col: Int, value: Double)(implicit ev: MatrixOperationsTC[A]): A = ev.fill(row, col, value)

  def csvRead[A](fn: String)(implicit ev: MatrixOperationsTC[A]): A = ev.csvRead(fn)

  def eigen[A](lhs: A)(implicit ev: MatrixOperationsTC[A]): (A, A) = {
    val e = ev.eig(lhs); (ev.values(e), ev.vectors(e))
  }

}
