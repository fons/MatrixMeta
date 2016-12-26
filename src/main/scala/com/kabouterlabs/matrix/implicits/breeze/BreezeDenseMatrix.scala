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

import spire.math.{Complex, Numeric}


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
  type ElemT         = Double
  type MatrixT       = DenseMatrix[ElemT]
  type MatrixMonT    = MatrixM[MatrixT]
  type EigenResultT  = breeze.linalg.eig.DenseEig


  private def boolToDouble(b: Boolean): Double = if (b) 1.0 else 0.0

  implicit class Ev$SizeT(matrix:MatrixMonT) extends SizeT {
    override val rows: Int = matrix.map(_.rows)
    override val columns: Int = matrix.map(_.cols)
    override val size: Int = matrix.map(_.size)
    override val isNull: Boolean = matrix.matrix match {
      case Some(_) => false
      case None    => true
    }
  }

  implicit class Ev$FormatterT(matrix:MatrixMonT) extends FormatterT {
    override def stringefy = matrix.matrix match {
      case Some(m) =>  "{" + m.getClass.getName + "  " + matrix.rows + " * " + matrix.columns + "\n" + m.toString + "}"
      case None    => "{none}"
    }

  }

  implicit val ev$FactoryT = new FactoryT {
    override type MatrixImplT = DenseMatrix[Double]

    implicit override def apply(row: Int, col: Int, data: Array[Double]): Option[MatrixImplT] = BreezeDenseMatrix(row, col, data)

    implicit override def apply(row: Int, col: Int): Option[MatrixImplT] = BreezeDenseMatrix(row, col)
  }

  implicit object Ev$CompanionT extends CompanionT {
    override type MatrixImplT = MatrixMonT

    override def fill(row: Int, col: Int, value: ElemT): MatrixMonT = MatrixM(row, col, Array.fill[Double](row * col)(value))

    override def ones(row: Int, col: Int): MatrixMonT = MatrixM.safeMap(()=>{DenseMatrix.ones(row, col)})

    override def rand(row: Int, col: Int): MatrixMonT = MatrixM({DenseMatrix.rand[Double](row, col)})

    override def eye(size: Int): MatrixMonT = MatrixM({DenseMatrix.eye[Double](size)})

    override def diag(data: Array[Double]): MatrixMonT = MatrixM({breeze.linalg.diag(new DenseVector[Double](data))})

    override def zeros(row: Int, col: Int): MatrixMonT = MatrixM({DenseMatrix.zeros[Double](row, col)})
  }


  implicit class Ev$AggregationT(matrix: MatrixMonT) extends AggregateT[MatrixMonT] {
    override def sumRows: MatrixMonT = matrix.map1((m: MatrixT) => breeze.linalg.sum(m(::, *)).toDenseMatrix)

    override def sumCols: MatrixMonT = matrix.map1((m: MatrixT) => breeze.linalg.sum(m(*, ::)).toDenseMatrix.t)

    override def trace: Option[ElemT] = matrix.safeMap(breeze.linalg.trace(_))

    override def sum: Option[ElemT] = matrix.safeMap(breeze.linalg.sum(_))

  }


  implicit class Ev$SliceT(matrix: MatrixMonT) extends SliceT[MatrixMonT] {

    override def deepCopy: MatrixMonT = matrix.map1(_.copy)

    override def apply(row: Int, coll: Int, v: ElemT): MatrixMonT = matrix.map1((m) => {m(row to row, coll to coll) := v; m})

    override def toDiag: MatrixMonT = matrix.map1((m: MatrixT) => m :* DenseMatrix.eye[Double](m.rows))

    override def toArray: Option[Array[ElemT]] = matrix.safeMap(_.toArray)

    def name = "breeze double matrix" + matrix.toString

    // breeze supports step 1 size only
    override def apply[K, L](row: K, col: L): MatrixMonT = (row, col) match {
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

    override def concatRight(rhs: MatrixMonT): MatrixMonT = for (l<- matrix; r <-rhs) yield MatrixM({DenseMatrix.horzcat(l,r)})


    override def concatDown(rhs: MatrixMonT): MatrixMonT = for (l<- matrix; r <-rhs) yield MatrixM({DenseMatrix.vertcat(l,r)})

  }

  class EigenAccess extends EigenAccessT[EigenResultT] {
    override def name = "apache commons result"
    type EigenVectorRetT = Option[EigenAccessT[EigenResultT]#EigenVectorT]
    type EigenValueRetT  = Option[EigenAccessT[EigenResultT]#EigenValuesT]

    override def vectors(result: Option[EigenResultT]): EigenVectorRetT = for (e <- result) yield {
      (for (col <- Range(0, e.eigenvalues.length)) yield {
        (for (row <- Range(0, e.eigenvalues.length)) yield {
          if (e.eigenvectorsComplex(col) == 0.0) Complex(e.eigenvectors(row, col), 0.0)
          else {
            val (offset1, offset2, factor) = if ((col % 2) == 0) (0, 1, 1.0) else (-1, 0, -1.0)
            Complex(e.eigenvectors(row, col + offset1), factor * e.eigenvectors(row, col + offset2))
          }
        }).toVector
      }).toArray
    }

    override def values(result:Option[EigenResultT]):EigenValueRetT = for ( e <- result) yield {
      for ((r,i) <- e.eigenvalues.data.zip(e.eigenvectorsComplex.data)) yield Complex(r,i)
    }


  }
  implicit class Ev$LinearAlgebraT(matrix: MatrixMonT) extends LinearAlgebraT {
    implicit val access = new EigenAccess
    override type MatrixRetTypeT = MatrixMonT
    override type EigenResultRetTypeT = EigenResultM[EigenResultT]

    override def eig: EigenResultRetTypeT = for (m <- matrix)  yield  EigenResultM({breeze.linalg.eig(m)})

    override def solve(rhs: MatrixMonT): MatrixMonT = for (l<- matrix; r <-rhs) yield MatrixM({l \ r})

    override def inverse: MatrixRetTypeT = for (m<-matrix) yield MatrixM({inv(m)})//matrix.flatMap(inv(_))

    override def transpose: MatrixMonT = matrix.map1(_.t)

    override def determinant: Option[ElemT] = matrix.safeMap(det(_))
  }

  implicit object Ev$SerializeT extends SerializeT[MatrixMonT] {
    override def csvWrite(fn: String, matrix: MatrixMonT): Unit = matrix.map(breeze.linalg.csvwrite(new java.io.File(fn), _))
    override def csvRead(fn: String): MatrixMonT = MatrixM({breeze.linalg.csvread(new File(fn))})
  }



  implicit object Ev$MatrixOperationsTC extends MatrixOperationsTC[MatrixMonT] {

    override def deepCopy(lhs: MatrixMonT): MatrixMonT = lhs.deepCopy

    override type EigenResultRetTypeT = EigenResultM[EigenResultT]

    override def eig(m: MatrixMonT): EigenResultRetTypeT = m.eig

    override def vectors(r: EigenResultM[EigenResultT]): Option[EigenResultRetTypeT#EigenVectorT] = r.vectors

    override def values(r: EigenResultM[EigenResultT]): Option[EigenResultRetTypeT#EigenValuesT] = r.values

    override def eigen(m: MatrixMonT): (Option[EigenResultRetTypeT#EigenValuesT]  ,Option[EigenResultRetTypeT#EigenVectorT] ) = {val e = m.eig; (e.values, e.vectors)}

    override def add(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (l <-lhs; r <-rhs) yield MatrixM({l+r})

    override def sub(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (l <-lhs; r <-rhs) yield MatrixM({l-r})

    override def multe(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (l <-lhs; r <-rhs) yield MatrixM({l:*r})

    override def dive(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT =  for (l <-lhs; r <-rhs) yield MatrixM({l:/r})

    override def mult(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (l <-lhs; r <-rhs) yield MatrixM({l*r})

    override def add1[B: Numeric](lhs: MatrixMonT, rhs: B) = for (lhsm<- lhs) yield MatrixM({BreezeDenseMatrix.add(lhsm.copy,rhs)})

    override def sub1[B: Numeric](lhs: MatrixMonT, rhs: B) = for (lhsm<- lhs) yield MatrixM({BreezeDenseMatrix.sub(lhsm.copy,rhs)})

    override def mul1[B: Numeric](lhs: MatrixMonT, rhs: B) = for (lhsm<- lhs) yield MatrixM({BreezeDenseMatrix.mul(lhsm.copy,rhs)})

    override def div1[B: Numeric](lhs: MatrixMonT, rhs: B) = for (lhsm<- lhs) yield MatrixM({BreezeDenseMatrix.div(lhsm.copy,rhs)})

    override def eq(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT =  for (l <-lhs; r <-rhs) yield MatrixM({  (l :== r).mapValues(boolToDouble)})

    override def ne(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT =  for (l <-lhs; r <-rhs) yield MatrixM({  (!(l :== r)).mapValues(boolToDouble)})

    override def lt(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (l <-lhs; r <-rhs) yield MatrixM({  ((l :< r)).mapValues(boolToDouble)})

    override def le(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (l <-lhs; r <-rhs) yield MatrixM({  ((l :< r) :|( l:==r)).mapValues(boolToDouble)})

    override def ge(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (l <-lhs; r <-rhs) yield MatrixM({  (!(l :< r)).mapValues(boolToDouble)})

    override def gt(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT =
      for (l <-lhs; r <-rhs) yield MatrixM({  (!((l :< r) :|( l:==r))).mapValues(boolToDouble)})

    override def create(rows: Int, colls: Int, data: Array[ElemT]): MatrixMonT = MatrixM(rows, colls, data)

    override def create(rows: Int, colls: Int): MatrixMonT = MatrixM(rows, colls)

    override def zero(row: Int, col: Int): MatrixMonT = MatrixM.zero(row, col)

    override def rand(row: Int, col: Int): MatrixMonT = MatrixM.rand(row, col)

    override def eye(size: Int): MatrixMonT = MatrixM.eye(size)

    override def diag(data: Array[ElemT]): MatrixMonT = MatrixM.diag(data)

    override def one(row: Int, col: Int): MatrixMonT = MatrixM.one(row, col)

    override def fill(row: Int, col: Int, value: ElemT): MatrixMonT = MatrixM.fill(row, col, value)

    override def inverse(m: MatrixMonT): MatrixMonT = m.inverse

    override def solve(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = lhs.solve(rhs)

    override def transpose(m: MatrixMonT): MatrixMonT = m.transpose

    override def determinant(m: MatrixMonT): Option[ElemT] = m.determinant

    override def get(m: MatrixMonT, row: Int, coll: Int): Option[ElemT] = m(row, coll)

    override def concatDown(m: MatrixMonT, down: MatrixMonT): MatrixMonT = m concatDown down

    override def set(m: MatrixMonT, row: Int, coll: Int, v: ElemT): MatrixMonT = m(row, coll, v)

    override def toArray(m: MatrixMonT): Option[Array[ElemT]] = m.toArray

    override def concatRight(m: MatrixMonT, rhs: MatrixMonT): MatrixMonT = m concatRight rhs

    override def toDiag(m: MatrixMonT): MatrixMonT = m.toDiag

    override def slice[K, L](m: MatrixMonT, row: K, col: L): MatrixMonT = m(row, col)

    override def csvWrite(fn: String, u: MatrixMonT): Unit = MatrixM.csvwrite(fn, u)

    override def csvRead(fn: String): MatrixMonT = MatrixM.csvread(fn)

    override def sumRows(m: MatrixMonT): MatrixMonT = m.sumRows

    override def sumCols(m: MatrixMonT): MatrixMonT = m.sumCols

    override def sum(m: MatrixMonT): Option[ElemT] = m.sum

    override def trace(m: MatrixMonT): Option[ElemT] = m.trace

    override def none  = MatrixM.none
  }

}

