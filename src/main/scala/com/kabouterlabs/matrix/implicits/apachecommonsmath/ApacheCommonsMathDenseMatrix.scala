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

package com.kabouterlabs.matrix.implicits.apachecommonsmath

/**
  * Created by fons on 12/12/16.
  */

import java.io.{PrintWriter, StringWriter, File}

import com.kabouterlabs.matrix._

import com.kabouterlabs.matrix.MatrixOperations.MatrixOperationsTC


import org.apache.commons.math3.linear._


import spire.math.Numeric
import scala.io.Source
import scala.util.Random

/*
The array is column major order but the apache lib expects row major order.
 */
private object ApacheCommonsMathDenseMatrix {

  def apply(rows: Int, colls: Int, f: => Array[Double]): Option[RealMatrix] = {
    try {
      val a = (for (i <- Range(0, rows)) yield Range(i, rows * colls, rows).map(f(_)).toArray).toArray
      Some(MatrixUtils.createRealMatrix(a))
    }
    catch {
      case e: Throwable =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
    }
  }

  def apply(rows: Int, colls: Int): Option[RealMatrix] = {
    try {
      Some(MatrixUtils.createRealMatrix(rows, colls))
    }
    catch {
      case e: Throwable =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
    }
  }

  def lusolver(m: RealMatrix): Option[LUDecomposition] = {
    try {
      Some(new LUDecomposition(m))
    }
    catch {
      case e: Throwable =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
    }
  }

  def qrsolver(m: RealMatrix): Option[QRDecomposition] = {
    try {
      Some(new QRDecomposition(m))
    }
    catch {
      case e: Throwable =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
    }
  }

  def eigensolver(m: RealMatrix): Option[EigenDecomposition] = {
    try {
      Some(new EigenDecomposition(m))
    }
    catch {
      case e: Throwable =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
    }
  }

  //
  def csvwrite(file: File, matrix: RealMatrix): Unit = {
    val pw = new PrintWriter(file)
    for (i <- Range(0, matrix.getRowDimension)) pw.write(matrix.getRow(i).mkString(",") ++ "\n")
    pw.close()
  }

  def csvread(file: File): RealMatrix = {
    val l = (for (line <- Source.fromFile(file).getLines()) yield line.split(",").map((s) => s.toDouble)).toArray
    MatrixUtils.createRealMatrix(l.toArray)
  }

  def add[B: Numeric](matrix: RealMatrix, rhs: B): RealMatrix = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.scalarAdd(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.scalarAdd(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.scalarAdd(rhs.asInstanceOf[Int])
    case q if rhs.isInstanceOf[Int] => matrix.scalarAdd(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def sub[B: Numeric](matrix: RealMatrix, rhs: B): RealMatrix = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.scalarAdd(-1 * rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.scalarAdd(-1 * rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.scalarAdd(-1 * rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def mul[B: Numeric](matrix: RealMatrix, rhs: B): RealMatrix = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.scalarMultiply(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.scalarMultiply(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.scalarMultiply(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def div[B: Numeric](matrix: RealMatrix, rhs: B): RealMatrix = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.scalarMultiply(1.0 / rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.scalarMultiply(1.0 / rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.scalarMultiply(1.0 / rhs.asInstanceOf[Int])
    case _ => matrix
  }

}

object ApacheCommonsMathDenseMatrixImplicit {
  type ElemT = Double
  type MatrixImpl = RealMatrix
  type MatrixDouble = MatrixM[MatrixImpl]
  type CommonsMathEigenResultT = EigenResultM[(RealMatrix, RealMatrix)]

  private def @#(matrix: MatrixDouble, a1: Int, a2: Int, a3: Int, a4: Int, f: (MatrixImpl, Int, Int, Int, Int) => MatrixImpl): MatrixDouble =
    for (m <- matrix) yield MatrixM({
      f(m, a1, a2, a3, a4)
    })

  //--------------------------------------------------------------------------------------------------------------
  //
  implicit val fdouble$ = new FactoryT {
    type MatrixImplT = MatrixImpl

    implicit override def apply(row: Int, col: Int, data: Array[Double]): Option[MatrixImplT] = ApacheCommonsMathDenseMatrix(row, col, data)

    implicit override def apply(row: Int, col: Int): Option[MatrixImplT] = ApacheCommonsMathDenseMatrix(row, col)

  }

  implicit object CommonsMathCompanion extends CompanionT {
    override type MatrixImplT = MatrixDouble

    override def fill(row: Int, col: Int, value: ElemT): MatrixDouble = MatrixM({
      ApacheCommonsMathDenseMatrix(row, col, {
        Array.fill[Double](row * col)(value)
      })
    })

    override def ones(row: Int, col: Int): MatrixDouble = fill(row, col, 1.0)

    override def rand(row: Int, col: Int): MatrixImplT = MatrixM({
      ApacheCommonsMathDenseMatrix(row, col, {
        Array.fill[Double](row * col)(Random.nextDouble())
      })
    })


    override def eye(size: Int): MatrixImplT = MatrixM({
      MatrixUtils.createRealIdentityMatrix(size)
    })

    override def zeros(row: Int, col: Int): MatrixImplT = fill(row, col, 0.0)

    //TODO check this
    override def diag(data: Array[Double]): MatrixImplT = {
      fdouble$(data.length, 1, data) match {
        case None => MatrixM.none
        case Some(a) => MatrixM({
          MatrixUtils.createRealDiagonalMatrix(data)
        })
      }
    }
  }


  implicit class AggregationT$implicit(matrix: MatrixDouble) extends AggregateT[MatrixDouble] {
    override def sumRows: MatrixDouble = for (m <- matrix) yield
      MatrixM(1, m.getColumnDimension, {(for (i <- Range(0, m.getColumnDimension)) yield m.getColumn(i).sum).toArray})

    override def sumCols: MatrixDouble = for (m <- matrix) yield
      MatrixM(m.getRowDimension,1,{(for (i <- Range(0, m.getRowDimension)) yield m.getRow(i).sum).toArray })

    override def trace = matrix.safeMap(_.getTrace)

    override def sum: Option[ElemT] = matrix.safeMap(_.getData.flatten.sum)

  }

  implicit class SliceT$implicit(matrix: MatrixDouble) extends SliceT[MatrixDouble] {
    private def @#(matrix: MatrixDouble, a1: Int, a2: Int, a3: Int, a4: Int, f: (MatrixImpl, Int, Int, Int, Int) => MatrixImpl): MatrixDouble =
      for (m <- matrix) yield MatrixM({
        f(m, a1, a2, a3, a4)
      })

    override def deepCopy: MatrixDouble = for (m <- matrix) yield MatrixM(m.getRowDimension, m.getColumnDimension, m.getData.transpose.flatten)

    override def apply(row: Int, coll: Int, v: ElemT): MatrixDouble = matrix.map1((m: RealMatrix) => {
      m.setEntry(row, coll, v); m
    })

    override def toDiag: MatrixDouble = for (m <- matrix) yield MatrixM({
      MatrixUtils.createRealDiagonalMatrix(Range(0, m.getRowDimension).zip(Range(0, m.getColumnDimension)).map(f => m.getEntry(f._1, f._2)).toArray)
    })

    override def concatRight(rhs: MatrixDouble): MatrixDouble = for (l <- matrix; r <- rhs) yield {
      MatrixM({
        for (m <- ApacheCommonsMathDenseMatrix(l.getRowDimension, l.getColumnDimension + r.getColumnDimension)) yield {
          m.setSubMatrix(l.getData, 0, 0);
          m.setSubMatrix(r.getData, 0, l.getColumnDimension)
          m;
        }
      })
    }

    override def concatDown(rhs: MatrixDouble): MatrixDouble = for (l <- matrix; r <- rhs) yield {
      MatrixM({
        for (m <- ApacheCommonsMathDenseMatrix(l.getRowDimension + r.getRowDimension, l.getColumnDimension)) yield {
          m.setSubMatrix(l.getData, 0, 0);
          m.setSubMatrix(r.getData, l.getRowDimension, 0);
          m
        }
      })
    }

    //GetData gets the data in row major order; we want column major order
    override def toArray: Option[Array[ElemT]] = matrix.safeMap(_.transpose.getData.flatten)

    override def apply(row: Int, coll: Int): Option[Double] = matrix.safeMap(_.getEntry(row, coll))

    override def apply[K, L](row: K, col: L): MatrixDouble = (row, col) match {
      case (r: Range, `::`) => @#(matrix, r.start, r.end, 0, 0, (m, start: Int, end: Int, _, _) => m.getSubMatrix(r.start, r.end, 0, m.getColumnDimension - 1))
      case (`::`, r: Range) => @#(matrix, r.start, r.end, 0, 0, (m, start: Int, end: Int, _, _) => m.getSubMatrix(0, m.getRowDimension - 1, start, end))
      case (row: Int, `::`) => @#(matrix, row, 0, 0, 0, (m: RealMatrix, row: Int, _, _, _) => m.getSubMatrix(row, row, 0, m.getColumnDimension - 1))
      case (`::`, col: Int) => @#(matrix, col, 0, 0, 0, (m: RealMatrix, col: Int, _, _, _) => m.getSubMatrix(0, m.getRowDimension - 1, col, col))
      case (r: Range, c: Range) => @#(matrix, r.start, r.end + 1, c.start, c.end + 1, (m: RealMatrix, rs, re, cs, ce) => m.getSubMatrix(rs, re, cs, ce))
      case (row: Int, r: Range) => @#(matrix, row, row + 1, r.start, r.end + 1, (m: RealMatrix, rs, re, cs, ce) => m.getSubMatrix(rs, re, cs, ce))
      case (r: Range, col: Int) => @#(matrix, r.start, r.end + 1, col, col + 1, (m: RealMatrix, rs, re, cs, ce) => m.getSubMatrix(rs, re, cs, ce))
      case (_, _) => matrix
    }
  }


  implicit class LinearAlgebraT$implicit(matrix: MatrixDouble) extends LinearAlgebraT {
    private val lu = for (m <- matrix) yield ApacheCommonsMathDenseMatrix.lusolver(m)
    private val qr = for (m <- matrix) yield ApacheCommonsMathDenseMatrix.qrsolver(m)
    private val ei = for (m <- matrix) yield ApacheCommonsMathDenseMatrix.eigensolver(m)

    override type MatrixRetTypeT = MatrixDouble
    override type EigenResultT = CommonsMathEigenResultT

    override def inverse: MatrixRetTypeT = MatrixM({
      matrix.safeMap((m) => qr.map(_.getSolver.getInverse)).flatten
    })

    //TODO : can be optimized based on matrix type..
    override def solve(rhs: MatrixDouble): MatrixRetTypeT = for (l <- matrix; r <- rhs) yield MatrixM({
      lu.map(_.getSolver.solve(r))
    })

    override def eig: EigenResultT = EigenResultM({
      for (e <- ei) yield (e.getV, e.getD)
    })

    override def transpose: MatrixRetTypeT = matrix.map1((m: RealMatrix) => m.transpose())

    override def determinant: Option[Double] = lu.map(_.getDeterminant)

  }

  implicit object SerializeT$implicit extends SerializeT[MatrixDouble] {
    override def csvWrite(fn: String, matrix: MatrixDouble): Unit = matrix.map(ApacheCommonsMathDenseMatrix.csvwrite(new File(fn), _))

    override def csvRead(fn: String): MatrixDouble = MatrixM({
      ApacheCommonsMathDenseMatrix.csvread(new File(fn))
    })
  }


  //TODO : Complex eigenvalues/eigenvectors aren't handled

  implicit class EigenAccessT$implicit(result: CommonsMathEigenResultT) extends EigenAccessT[MatrixDouble] {
    def name = "ApacheCommonsMath result"

    override def vectors: MatrixDouble = MatrixM({
      for (r <- result) yield r._1
    })

    override def values: MatrixDouble = MatrixM({
      for (r <- result) yield r._2
    })

  }


  implicit object MatrixOperationsTC$implicit$ extends MatrixOperationsTC[MatrixDouble] {

    private def op(lhs: MatrixDouble, rhs: MatrixDouble, f: (Double, Double) => Double): MatrixDouble =
      for (l <- lhs; r <- rhs) yield {
        val la = l.transpose.getData.flatten
        val lb = r.transpose.getData.flatten
        MatrixM({
          ApacheCommonsMathDenseMatrix(l.getRowDimension, l.getColumnDimension,
            la.zip(lb).map(e => f(e._1, e._2)))
        })
      }


    private def opb(lhs: MatrixDouble, rhs: MatrixDouble, f: (Double, Double) => Boolean): MatrixDouble =
      for (l <- lhs; r <- rhs) yield {
        val la = l.transpose.getData.flatten
        val lb = r.transpose.getData.flatten
        MatrixM({
          ApacheCommonsMathDenseMatrix(l.getRowDimension, l.getColumnDimension,
            la.zip(lb).map(e => if (f(e._1, e._2)) 1.0 else 0.0))
        })
      }


    override def deepCopy(lhs: MatrixDouble): MatrixDouble = lhs.deepCopy

    override type EigenResultT = Option[Double]

    override def eig(m: MatrixDouble): Option[Double] = None

    override def vectors(r: EigenResultT): MatrixDouble = MatrixM.none

    override def values(r: EigenResultT): MatrixDouble = MatrixM.none

    override def eigen(m: MatrixDouble): (MatrixDouble, MatrixDouble) = (MatrixM.none, MatrixM.none)

    override def add(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({
      lhsm.add(rhsm)
    })

    override def sub(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({
      lhsm.subtract(rhsm)
    })

    override def mult(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({
      lhsm.multiply(rhsm)
    })

    override def multe(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = op(lhs, rhs, _ * _)

    override def dive(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = op(lhs, rhs, _ / _)


    override def add1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble = for (lhsm <- lhs) yield MatrixM({
      ApacheCommonsMathDenseMatrix.add(lhsm, rhs)
    })

    override def sub1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble = for (lhsm <- lhs) yield MatrixM({
      ApacheCommonsMathDenseMatrix.sub(lhsm, rhs)
    })

    override def mul1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble = for (lhsm <- lhs) yield MatrixM({
      ApacheCommonsMathDenseMatrix.mul(lhsm, rhs)
    })

    override def div1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble = for (lhsm <- lhs) yield MatrixM({
      ApacheCommonsMathDenseMatrix.div(lhsm, rhs)
    })

    override def eq(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = opb(lhs, rhs, _ == _)

    override def ne(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = opb(lhs, rhs, _ != _)

    override def gt(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = opb(lhs, rhs, _ > _)

    override def ge(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = opb(lhs, rhs, _ >= _)

    override def lt(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = opb(lhs, rhs, _ < _)

    override def le(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = opb(lhs, rhs, _ <= _)

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

    override def none = MatrixM.none

  }

}

