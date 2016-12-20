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

package com.kabouterlabs.matrix.implicits.breeze

import java.io.{File, PrintWriter, StringWriter}


import breeze.linalg.{DenseMatrix, DenseVector, det, inv, *}
import com.kabouterlabs.matrix.MatrixOperations._

import com.kabouterlabs.matrix._

import spire.math.Numeric


/**
  * Created by fons on 3/21/16.
  */


private object BreezeDenseMatrix {

  def apply[B](rows: Int, colls: Int, data: Array[B]): Option[DenseMatrix[B]] = {
    try {
      Some(new DenseMatrix[B](rows, colls, data))
    }
    catch {
      case e: Throwable =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
    }
  }

  def apply(rows: Int, cols: Int): Option[DenseMatrix[Double]] = {
    try {
      Some(DenseMatrix.zeros[Double](rows, cols))
    }
    catch {
      case e: Throwable =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
    }
  }

  def add[B: Numeric](matrix: DenseMatrix[Double], rhs: B): DenseMatrix[Double] = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix :+= rhs.asInstanceOf[Double]
    case q if rhs.isInstanceOf[Float] => matrix :+= rhs.asInstanceOf[Float].toDouble
    case q if rhs.isInstanceOf[Int] => matrix :+= rhs.asInstanceOf[Int].toDouble
    case _ => matrix
  }


  def sub[B: Numeric](matrix: DenseMatrix[Double], rhs: B): DenseMatrix[Double] = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix :+= (-1.0 * rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix :+= (-1.0 * rhs.asInstanceOf[Float].toDouble)
    case q if rhs.isInstanceOf[Int] => matrix :+= (-1.0 * rhs.asInstanceOf[Int].toDouble)
    case _ => matrix
  }

  def mul[B: Numeric](matrix: DenseMatrix[Double], rhs: B): DenseMatrix[Double] = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix :*= rhs.asInstanceOf[Double]
    case q if rhs.isInstanceOf[Float] => matrix :*= rhs.asInstanceOf[Float].toDouble
    case q if rhs.isInstanceOf[Int] => matrix :*= rhs.asInstanceOf[Int].toDouble
    case _ => matrix
  }

  def div[B: Numeric](matrix: DenseMatrix[Double], rhs: B): DenseMatrix[Double] = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix :*= (1.0 / rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix :*= (1.0 / rhs.asInstanceOf[Float].toDouble)
    case q if rhs.isInstanceOf[Int] => matrix :*= (1.0 / rhs.asInstanceOf[Int].toDouble)
    case _ => matrix
  }

}


// Per-companion boilerplate for access via implicit resolution


object BreezeDenseMatrixImplicit {
  type ElemT = Double
  type MatrixImpl = DenseMatrix[ElemT]
  type MatrixDouble = MatrixM[MatrixImpl]
  type BreezeEigenResult = EigenResultM[breeze.linalg.eig.DenseEig]


  private def boolToDouble(b: Boolean): Double = if (b) 1.0 else 0.0

  implicit class BreezeDenseMatrixSizeT$Ev(matrix:MatrixDouble) extends SizeT {
    override val rows: Int = matrix.map(_.rows)
    override val columns: Int = matrix.map(_.cols)
    override val size: Int = matrix.map(_.size)
    override val isNull: Boolean = matrix.matrix match {
      case Some(_) => false
      case None    => true
    }
  }

  implicit class BaseFormatter$Breeze(matrix:MatrixDouble) extends FormatterT {
    override def stringefy = matrix.matrix match {
      case Some(m) =>  "{" + m.getClass.getName + "  " + matrix.rows + " * " + matrix.columns + "\n" + m.toString + "}"
      case None    => "{none}"
    }

  }

  implicit val fdouble = new FactoryT {
    override type MatrixImplT = DenseMatrix[Double]

    implicit override def apply(row: Int, col: Int, data: Array[Double]): Option[MatrixImplT] = BreezeDenseMatrix(row, col, data)

    implicit override def apply(row: Int, col: Int): Option[MatrixImplT] = BreezeDenseMatrix(row, col)
  }

  implicit object CompanionT$implicit extends CompanionT {
    override type MatrixImplT = MatrixDouble

    override def fill(row: Int, col: Int, value: ElemT): MatrixDouble = MatrixM(row, col, Array.fill[Double](row * col)(value))

    override def ones(row: Int, col: Int): MatrixDouble = MatrixM.safeMap(()=>{DenseMatrix.ones(row, col)})

    override def rand(row: Int, col: Int): MatrixDouble = MatrixM({DenseMatrix.rand[Double](row, col)})

    override def eye(size: Int): MatrixDouble = MatrixM({DenseMatrix.eye[Double](size)})

    override def diag(data: Array[Double]): MatrixDouble = MatrixM({breeze.linalg.diag(new DenseVector[Double](data))})

    override def zeros(row: Int, col: Int): MatrixDouble = MatrixM({DenseMatrix.zeros[Double](row, col)})
  }


  implicit class AggregationT$implicit(matrix: MatrixDouble) extends AggregateT[MatrixDouble] {
    override def sumRows: MatrixDouble = matrix.map1((m: MatrixImpl) => breeze.linalg.sum(m(::, *)).toDenseMatrix)

    override def sumCols: MatrixDouble = matrix.map1((m: MatrixImpl) => breeze.linalg.sum(m(*, ::)).toDenseMatrix.t)

    override def trace: Option[ElemT] = matrix.safeMap(breeze.linalg.trace(_))

    override def sum: Option[ElemT] = matrix.safeMap(breeze.linalg.sum(_))

  }


  implicit class SliceT$implicit(matrix: MatrixDouble) extends SliceT[MatrixDouble] {

    override def deepCopy: MatrixDouble = matrix.map1(_.copy)

    override def apply(row: Int, coll: Int, v: ElemT): MatrixDouble = matrix.map1((m) => {m(row to row, coll to coll) := v; m})

    override def toDiag: MatrixDouble = matrix.map1((m: MatrixImpl) => m :* DenseMatrix.eye[Double](m.rows))

    override def toArray: Option[Array[ElemT]] = matrix.safeMap(_.toArray)

    def name = "breeze double matrix" + matrix.toString

    // breeze supports step 1 size only
    override def apply[K, L](row: K, col: L): MatrixDouble = (row, col) match {
      case (r: Range, ::) => matrix.map1(_(r.start to r.end, ::))
      case (::, r: Range) => matrix.map1(_(::, r.start to r.end))

      case (row: Int, ::) => matrix.map1(_(row, ::).t.toDenseMatrix)
      case (::, col: Int) => matrix.map1(_(::, col).toDenseMatrix.t)

      case (r: Range, c: Range) => matrix.map1(_(r.start to r.end, c.start to c.end))
      case (row: Int, r: Range) => matrix.map1(_(row, r.start to r.end).t.toDenseMatrix)
      case (r: Range, col: Int) => matrix.map1(_(r.start to r.end, col).toDenseMatrix)
      case (_, _) => matrix
    }
    override def apply(row: Int, col: Int): Option[ElemT] = matrix.safeMap(_(row,col))

    override def concatRight(rhs: MatrixDouble): MatrixDouble = for (l<- matrix; r <-rhs) yield MatrixM({DenseMatrix.horzcat(l,r)})


    override def concatDown(rhs: MatrixDouble): MatrixDouble = for (l<- matrix; r <-rhs) yield MatrixM({DenseMatrix.vertcat(l,r)})

  }

  implicit class LinearAlgebraT$implicit(matrix: MatrixDouble) extends LinearAlgebraT {
    override type MatrixRetTypeT = MatrixDouble
    override type EigenResultT = BreezeEigenResult

    override def eig: EigenResultT = for (m <- matrix)  yield  EigenResultM({breeze.linalg.eig(m)})

    override def solve(rhs: MatrixDouble): MatrixDouble = for (l<- matrix; r <-rhs) yield MatrixM({l \ r})

    override def inverse: MatrixRetTypeT = for (m<-matrix) yield MatrixM({inv(m)})//matrix.flatMap(inv(_))

    override def transpose: MatrixDouble = matrix.map1(_.t)

    override def determinant: Option[ElemT] = matrix.safeMap(det(_))
  }

  implicit object SerializeT$implicit extends SerializeT[MatrixDouble] {
    override def csvWrite(fn: String, matrix: MatrixDouble): Unit = matrix.map(breeze.linalg.csvwrite(new java.io.File(fn), _))
    override def csvRead(fn: String): MatrixDouble = MatrixM({breeze.linalg.csvread(new File(fn))})
  }

  //TODO : Complex eigenvalues/eigenvectors aren't handled
  implicit class EigenResultAccessT$(result: BreezeEigenResult) extends EigenAccessT[MatrixDouble] {
    def name = "jeigen breeze dense matrix result"

    override def values: MatrixDouble = MatrixM(result.result.map((r) => DenseMatrix.vertcat(r.eigenvalues.toDenseMatrix, r.eigenvectorsComplex.toDenseMatrix).t))

    //TODO : This will not handle complex eigen vectors.
    override def vectors: MatrixDouble = MatrixM(result.result.map((r) => r.eigenvectors))
  }

  implicit object MatrixOperationsTC$implicit$ extends MatrixOperationsTC[MatrixDouble] {

    override def deepCopy(lhs: MatrixDouble): MatrixDouble = lhs.deepCopy

    override type EigenResultT = BreezeEigenResult

    override def eig(m: MatrixDouble): EigenResultT = m.eig

    override def vectors(r: EigenResultT): MatrixDouble = r.vectors

    override def values(r: EigenResultT): MatrixDouble = r.values

    override def eigen(m:MatrixDouble): (MatrixDouble, MatrixDouble) = {val l = m.eig; (l.values, l.vectors)}

    override def add(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (l <-lhs; r <-rhs) yield MatrixM({l+r})

    override def sub(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (l <-lhs; r <-rhs) yield MatrixM({l-r})

    override def multe(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (l <-lhs; r <-rhs) yield MatrixM({l:*r})

    override def dive(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble =  for (l <-lhs; r <-rhs) yield MatrixM({l:/r})

    override def mult(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (l <-lhs; r <-rhs) yield MatrixM({l*r})

    override def add1[B: Numeric](lhs: MatrixDouble, rhs: B) = for (lhsm<- lhs) yield MatrixM({BreezeDenseMatrix.add(lhsm.copy,rhs)})

    override def sub1[B: Numeric](lhs: MatrixDouble, rhs: B) = for (lhsm<- lhs) yield MatrixM({BreezeDenseMatrix.sub(lhsm.copy,rhs)})

    override def mul1[B: Numeric](lhs: MatrixDouble, rhs: B) = for (lhsm<- lhs) yield MatrixM({BreezeDenseMatrix.mul(lhsm.copy,rhs)})

    override def div1[B: Numeric](lhs: MatrixDouble, rhs: B) = for (lhsm<- lhs) yield MatrixM({BreezeDenseMatrix.div(lhsm.copy,rhs)})

    override def eq(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble =  for (l <-lhs; r <-rhs) yield MatrixM({  (l :== r).mapValues(boolToDouble)})

    override def ne(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble =  for (l <-lhs; r <-rhs) yield MatrixM({  (!(l :== r)).mapValues(boolToDouble)})

    override def lt(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (l <-lhs; r <-rhs) yield MatrixM({  ((l :< r)).mapValues(boolToDouble)})

    override def le(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (l <-lhs; r <-rhs) yield MatrixM({  ((l :< r) :|( l:==r)).mapValues(boolToDouble)})

    override def ge(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (l <-lhs; r <-rhs) yield MatrixM({  (!(l :< r)).mapValues(boolToDouble)})

    override def gt(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble =
      for (l <-lhs; r <-rhs) yield MatrixM({  (!((l :< r) :|( l:==r))).mapValues(boolToDouble)})

    override def create(rows: Int, colls: Int, data: Array[ElemT]): MatrixDouble = MatrixM(rows, colls, data)

    override def create(rows: Int, colls: Int): MatrixDouble = MatrixM(rows, colls)

    override def zero(row: Int, col: Int): MatrixDouble = MatrixM.zero(row, col)

    override def rand(row: Int, col: Int): MatrixDouble = MatrixM.rand(row, col)

    override def eye(size: Int): MatrixDouble = MatrixM.eye(size)

    override def diag(data: Array[ElemT]): MatrixDouble = MatrixM.diag(data)

    override def one(row: Int, col: Int): MatrixDouble = MatrixM.one(row, col)

    override def fill(row: Int, col: Int, value: ElemT): MatrixDouble = MatrixM.fill(row, col, value)

    override def inverse(m: MatrixDouble): MatrixDouble = m.inverse

    override def solve(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = lhs.solve(rhs)

    override def transpose(m: MatrixDouble): MatrixDouble = m.transpose

    override def determinant(m: MatrixDouble): Option[ElemT] = m.determinant

    override def get(m: MatrixDouble, row: Int, coll: Int): Option[ElemT] = m(row, coll)

    override def concatDown(m: MatrixDouble, down: MatrixDouble): MatrixDouble = m concatDown down

    override def set(m: MatrixDouble, row: Int, coll: Int, v: ElemT): MatrixDouble = m(row, coll, v)

    override def toArray(m: MatrixDouble): Option[Array[ElemT]] = m.toArray

    override def concatRight(m: MatrixDouble, rhs: MatrixDouble): MatrixDouble = m concatRight rhs

    override def toDiag(m: MatrixDouble): MatrixDouble = m.toDiag

    override def slice[K, L](m: MatrixDouble, row: K, col: L): MatrixDouble = m(row, col)

    override def csvWrite(fn: String, u: MatrixDouble): Unit = MatrixM.csvwrite(fn, u)

    override def csvRead(fn: String): MatrixDouble = MatrixM.csvread(fn)

    override def sumRows(m: MatrixDouble): MatrixDouble = m.sumRows

    override def sumCols(m: MatrixDouble): MatrixDouble = m.sumCols

    override def sum(m: MatrixDouble): Option[ElemT] = m.sum

    override def trace(m: MatrixDouble): Option[ElemT] = m.trace

    override def none  = MatrixM.none
  }

}

