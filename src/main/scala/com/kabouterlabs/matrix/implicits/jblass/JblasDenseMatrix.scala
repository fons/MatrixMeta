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

package com.kabouterlabs.matrix.implicits.jblass

/**
  * Created by fons on 12/17/16.
  */


import java.io.{PrintWriter, StringWriter, File}

import com.kabouterlabs.matrix.MatrixExtension.MatrixExtensionsTC
import com.kabouterlabs.matrix.MatrixOperations.MatrixOperationsTC
import com.kabouterlabs.matrix._
import org.jblas.{ComplexDoubleMatrix, Eigen, Solve, DoubleMatrix}
import spire.math.{Complex, Numeric}
import spire.implicits._
import scala.io.Source

import com.kabouterlabs.matrix.implicits.extension.MatrixExtensionImplicit.MatrixMapper._
/**
  * Created by fons on 3/21/16.
  */


private object JblasDoubleMatrix {

  def apply(rows: Int, colls: Int, data: Array[Double]): Option[DoubleMatrix] = {
    try {
      val f = for (i <- Range(0, rows)) yield Range(i, rows * colls, rows).map(data(_)).toArray
      Some(new DoubleMatrix(f.toArray))
    }
    catch {
      case e: Throwable => {
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
      }
    }
  }

  def apply(rows: Int, colls: Int): Option[DoubleMatrix] = {
    try {

      Some(DoubleMatrix.zeros(rows, colls))
    }
    catch {
      case e: Throwable => {
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
      }
    }
  }


  def csvwrite(file: File, matrix: DoubleMatrix): Unit = {
    val pw = new PrintWriter(file)
    for (i <- Range(0, matrix.rows)) {
      pw.write(matrix.getRow(i).data.mkString(",") ++ "\n")
    }
    pw.close()
  }

  def csvread(file: File): DoubleMatrix = {
    val l = (for (line <- Source.fromFile(file).getLines()) yield {
      line.split(",").map((s) => s.toDouble)
    }).toArray
    new DoubleMatrix(l)
  }

  def add[B: Numeric](matrix: DoubleMatrix, rhs: B): DoubleMatrix = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.add(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.add(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.add(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def sub[B: Numeric](matrix: DoubleMatrix, rhs: B): DoubleMatrix = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.sub(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.sub(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.sub(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def mul[B: Numeric](matrix: DoubleMatrix, rhs: B): DoubleMatrix = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.mul(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.mul(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.mul(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def div[B: Numeric](matrix: DoubleMatrix, rhs: B): DoubleMatrix = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.div(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.div(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.div(rhs.asInstanceOf[Int])
    case _ => matrix
  }


}

object JblasDoubleMatrixImplicit {
  type ElemT = Double
  type MatrixT = DoubleMatrix
  type MatrixMonT = MatrixM[MatrixT]
  type EigenResultT = Array[ComplexDoubleMatrix]


  implicit class Ev$Formatter(matrix: MatrixMonT) extends FormatterT {

    override def stringefy: String = matrix.matrix match {
      case Some(m) => "{" + m.getClass.getName + " " + matrix.rows + " * " + matrix.columns + "\n" +
        m.toString.replace("[", "[ ").replace("]", " ]").replace(";", " ]\n[") + "}"
      case None => "{none}"
    }
  }

  implicit class Ev$SizeT(matrix: MatrixMonT) extends SizeT {
    override val rows: Int = matrix.map(_.rows)
    override val columns: Int = matrix.map(_.columns)
    override val size: Int = matrix.map(_.length)
    override val isNull: Boolean = matrix.matrix match {
      case Some(_) => false
      case None => true
    }

  }

  //--------------------------------------------------------------------------------------------------------------
  //
  implicit val ev$FactoryT = new FactoryT {
    type MatrixImplT = MatrixT

    implicit override def apply(row: Int, col: Int, data: Array[Double]): Option[MatrixImplT] = JblasDoubleMatrix(row, col, data)

    implicit override def apply(row: Int, col: Int): Option[MatrixImplT] = JblasDoubleMatrix(row, col)

  }

  implicit object Ev$Companion extends CompanionT {
    override type MatrixImplT = MatrixMonT

    override def fill(row: Int, col: Int, value: ElemT): MatrixMonT = MatrixM(row, col, Array.fill[Double](row * col)(value))

    override def ones(row: Int, col: Int): MatrixMonT = MatrixM({
      DoubleMatrix.ones(row, col)
    })

    override def rand(row: Int, col: Int): MatrixImplT = MatrixM({
      DoubleMatrix.rand(row, col)
    })

    override def eye(size: Int): MatrixImplT = MatrixM({
      DoubleMatrix.eye(size)
    })

    override def zeros(row: Int, col: Int): MatrixImplT = MatrixM({
      DoubleMatrix.zeros(row, col)
    })

    //TODO check this
    override def diag(data: Array[Double]): MatrixImplT = {
      ev$FactoryT(data.length, 1, data) match {
        case None => new MatrixMonT(None)
        case Some(a) => MatrixM.apply({
          DoubleMatrix.diag(a)
        })
      }
    }
  }


  implicit class Ev$AggregationT(matrix: MatrixMonT) extends AggregateT[MatrixMonT] {
    override def sumRows(): MatrixMonT = for (m <- matrix) yield MatrixM({
      m.columnSums()
    })

    override def sumCols(): MatrixMonT = for (m <- matrix) yield MatrixM({
      m.rowSums()
    })

    override def trace(): Option[ElemT] = matrix.safeMap(_.diag.sum)

    override def sum(): Option[ElemT] = matrix.safeMap(_.sum())
  }

  implicit class Ev$SliceT(matrix: MatrixMonT) extends SliceT[MatrixMonT] {
    private def @#(matrix: MatrixMonT, a1: Int, a2: Int, a3: Int, a4: Int, f: (MatrixT, Int, Int, Int, Int) => MatrixT): MatrixMonT =
      for (m <- matrix) yield MatrixM({
        f(m, a1, a2, a3, a4)
      })

    override def deepCopy: MatrixMonT = for (m <- matrix) yield MatrixM({
      m.dup()
    })

    override def apply(row: Int, coll: Int, v: ElemT): MatrixMonT = matrix.map1((m: DoubleMatrix) => {
      m.put(row, coll, v); m
    })

    override def toDiag: MatrixMonT = for (m <- matrix) yield MatrixM({
      DoubleMatrix.diag(m.diag())
    })

    override def concatRight(rhs: MatrixMonT): MatrixMonT = for (lhsm <- matrix; rhsm <- rhs) yield MatrixM({
      DoubleMatrix.concatHorizontally(lhsm, rhsm)
    })

    override def concatDown(rhs: MatrixMonT): MatrixMonT = for (lhsm <- matrix; rhsm <- rhs) yield MatrixM({
      DoubleMatrix.concatVertically(lhsm, rhsm)
    })

    override def toArray: Option[Array[ElemT]] = matrix.safeMap(_.toArray)

    override def apply(row: Int, coll: Int): Option[Double] = matrix.safeMap(_.get(row, coll))

    override def apply[K, L](row: K, col: L): MatrixMonT = (row, col) match {
      case (r: Range, `::`) => @#(matrix, r.start, r.end, 0, 0, (m: DoubleMatrix, start: Int, end: Int, _, _) => m.getRange(start, end + 1, 0, m.columns))
      case (`::`, r: Range) => @#(matrix, r.start, r.end, 0, 0, (m: DoubleMatrix, start: Int, end: Int, _, _) => m.getRange(0, m.rows, start, end + 1))
      case (row: Int, `::`) => @#(matrix, row, 0, 0, 0, (m: DoubleMatrix, row: Int, _, _, _) => m.getRow(row))
      case (`::`, col: Int) => @#(matrix, col, 0, 0, 0, (m: DoubleMatrix, col: Int, _, _, _) => m.getColumn(col))
      case (r: Range, c: Range) => @#(matrix, r.start, r.end + 1, c.start, c.end + 1, (m: DoubleMatrix, rs, re, cs, ce) => m.getRange(rs, re, cs, ce))
      case (row: Int, r: Range) => @#(matrix, row, row + 1, r.start, r.end + 1, (m: DoubleMatrix, rs, re, cs, ce) => m.getRange(rs, re, cs, ce))
      case (r: Range, col: Int) => @#(matrix, r.start, r.end + 1, col, col + 1, (m: DoubleMatrix, rs, re, cs, ce) => m.getRange(rs, re, cs, ce))
      case (_, _) => matrix
    }
  }

  class EigenAccess extends EigenAccessT[EigenResultT] {
    override def name = "jblas result"

    type EigenVectorRetT = Option[EigenVectorT]
    type EigenValueRetT = Option[EigenValuesT]

    override def vectors(result: Option[EigenResultT]): EigenVectorRetT = for (e <- result) yield {
      (for (j <- Range(0, e(0).columns)) yield {
        (for (k <- Range(0, e(0).rows)) yield {
          val d = e(0).getColumn(j).get(k); Complex(d.real(), d.imag())
        }).toVector
      }).toArray
    }

    override def values(result: Option[EigenResultT]): EigenValueRetT = for (e <- result) yield
      (for (i <- Range(0, e(1).rows)) yield {
        val d = e(1).get(i, i); Complex(d.real(), d.imag())
      }).toArray

  }

  implicit class Ev$LinearAlgebraT(matrix: MatrixMonT) extends LinearAlgebraT {
    implicit val ev$EigenAccess = new EigenAccess
    override type MatrixRetTypeT = MatrixMonT
    override type EigenResultRetTypeT = EigenResultM[EigenResultT]

    override def inverse: MatrixRetTypeT = for (m <- matrix) yield MatrixM({
      Solve.pinv(m)
    })

    //TODO : can be optimized based on matrix type..
    override def solve(rhs: MatrixMonT): MatrixRetTypeT = for (lhsm <- matrix; rhsm <- rhs) yield MatrixM({
      Solve.solve(lhsm, rhsm)
    })

    override def eig: EigenResultRetTypeT = for (m <- matrix) yield EigenResultM({
      Eigen.eigenvectors(m)
    })

    override def transpose: MatrixRetTypeT = matrix.map1(_.transpose())

    override def determinant: Option[Double] = for (a <- matrix.eig.values) yield a.foldLeft(Complex(1.0, 0.0))(_ * _).real


  }

  implicit object Ev$SerializeT extends SerializeT[MatrixMonT] {
    override def csvWrite(fn: String, matrix: MatrixMonT): Unit = matrix.map(JblasDoubleMatrix.csvwrite(new File(fn), _))

    override def csvRead(fn: String): MatrixMonT = MatrixM({
      JblasDoubleMatrix.csvread(new File(fn))
    })
  }

  implicit class Ev$SingularValueDecomposition(matrix: MatrixMonT) extends SingularValueDecompositionT[MatrixMonT] {
    override type SvdElemT = ElemT

    private case class SVD(U: MatrixMonT, Vt: MatrixMonT, S: Option[Array[ElemT]], Sm: MatrixMonT)

    private val svd_ = matrix.matrix match {
      case None => SVD(MatrixM.none, MatrixM.none, None, MatrixM.none)
      case Some(m) => {
        val svdr = org.jblas.Singular.fullSVD(m);
        val sm = MatrixM.zero(svdr(0).columns, svdr(2).rows)
        for (i <- Range(0, svdr(1).data.length)) sm(i, i, svdr(1).data(i))
        SVD(MatrixM({
          svdr(0)
        }), MatrixM({
          svdr(2)
        }).transpose, Some(svdr(1).toArray), sm)
      }
    }

    val svd = new SvdResultT {
      override def toString: String = super.toString + "\n" + "S=" + svd_.S.map(_.mkString(",")) + "\n" + svd_.U.stringefy + "\n" + svd_.Vt.stringefy

      override val U: MatrixMonT = svd_.U
      override val S: Option[Array[ElemT]] = svd_.S
      override val Vt: MatrixMonT = svd_.Vt

      override def Sm(): MatrixMonT = svd_.Sm

    }
  }

  implicit class Ev$QRDecompostion(matrix: MatrixMonT) extends QRDecompositionT[MatrixMonT] {

    private case class QR(q: MatrixMonT, r: MatrixMonT)

    private val qr_ = matrix.matrix match {
      case None => QR(MatrixM.none, MatrixM.none)
      case Some(m) => {
        val qrv = org.jblas.Decompose.qr(m)
        QR(MatrixM({
          qrv.q
        }), MatrixM({
          qrv.r
        }))
      }
    }

    val qr = new QRResultT {
      override lazy val toString: String = super.toString + "@" + super.hashCode() + "\n" + qr_.r.stringefy + "\n" + qr_.q.stringefy
      override val R: MatrixMonT = qr_.r
      override val Q: MatrixMonT = qr_.q
    }

  }

  implicit class Ev$LUDecompositionT(matrix: MatrixMonT) extends LUDecompositionT[MatrixMonT] {

    private case class LU(upper: MatrixMonT, lower: MatrixMonT, p: MatrixMonT)

    private val lu_ = matrix.matrix match {
      case None => LU(MatrixM.none, MatrixM.none, MatrixM.none)
      case Some(m) => val lur = org.jblas.Decompose.lu(m); LU(MatrixM({
        lur.u
      }), MatrixM({
        lur.l
      }), MatrixM({
        lur.p
      }))
    }

    val lu = new LUResultT {
      override def toString: String = super.toString + "@" + hashCode() + "\n" + lu_.lower.stringefy + "\n" + lu_.upper.stringefy

      override val L: MatrixMonT = lu_.lower
      override val U: MatrixMonT = lu_.upper
      override val P: MatrixMonT = lu_.p
      override val permutations: Option[Array[Int]] = None

      override def constructP(perms: Option[Array[Int]]): MatrixMonT = lu_.p
    }
  }

  implicit class Ev$CholeskyDecompositionT(matrix: MatrixMonT) extends CholeskyDecompositionT[MatrixMonT] {

    private case class Cholesky(chol: MatrixMonT)

    private val cholesky_ = matrix.matrix match {
      case None => Cholesky(MatrixM.none)
      case Some(m) => Cholesky(MatrixM({
        org.jblas.Decompose.cholesky(m)
      }).transpose)
    }

    val cholesky = new CholeskyResultT {
      override lazy val toString: String = super.toString + "@" + hashCode() + "\n" + cholesky_.chol.stringefy
      override val L: MatrixMonT = cholesky_.chol
    }
  }


  implicit object Ev$MatrixOperationsTC extends MatrixOperationsTC[MatrixMonT] {


    override type MatrixDataElemT = Double

    override def rows(m: MatrixMonT): Int = m.rows

    override def columns(m: MatrixMonT): Int = m.columns

    override def size(m: MatrixMonT): Int = m.size

    override def isNull(m: MatrixMonT): Boolean = m.isNull

    override def deepCopy(lhs: MatrixMonT): MatrixMonT = lhs.deepCopy

    override type EigenResultRetTypeT = EigenResultM[EigenResultT]

    override def eig(m: MatrixMonT): EigenResultRetTypeT = m.eig

    override def vectors(r: EigenResultM[EigenResultT]): Option[EigenResultRetTypeT#EigenVectorT] = r.vectors

    override def values(r: EigenResultM[EigenResultT]): Option[EigenResultRetTypeT#EigenValuesT] = r.values

    override def eigen(m: MatrixMonT): (Option[EigenResultRetTypeT#EigenValuesT], Option[EigenResultRetTypeT#EigenVectorT]) = {
      val e = m.eig; (e.values, e.vectors)
    }

    override def add(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield {
      MatrixM({
        lhsm.add(rhsm)
      })
    }

    override def sub(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield {
      MatrixM({
        lhsm.sub(rhsm)
      })
    }

    override def mult(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield {
      MatrixM({
        lhsm.mmul(rhsm)
      })
    }

    override def multe(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield {
      MatrixM({
        lhsm.mul(rhsm)
      })
    }

    override def dive(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield {
      MatrixM({
        lhsm.div(rhsm)
      })
    }

    override def add1[B: Numeric](lhs: MatrixMonT, rhs: B): MatrixMonT = for (lhsm <- lhs) yield MatrixM({
      JblasDoubleMatrix.add(lhsm, rhs)
    })

    override def sub1[B: Numeric](lhs: MatrixMonT, rhs: B): MatrixMonT = for (lhsm <- lhs) yield MatrixM({
      JblasDoubleMatrix.sub(lhsm, rhs)
    })

    override def mul1[B: Numeric](lhs: MatrixMonT, rhs: B): MatrixMonT = for (lhsm <- lhs) yield MatrixM({
      JblasDoubleMatrix.mul(lhsm, rhs)
    })

    override def div1[B: Numeric](lhs: MatrixMonT, rhs: B): MatrixMonT = for (lhsm <- lhs) yield MatrixM({
      JblasDoubleMatrix.div(lhsm, rhs)
    })

    override def eq(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({
      lhsm.eq(rhsm)
    })

    override def ne(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({
      lhsm.ne(rhsm)
    })

    override def gt(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({
      lhsm.gt(rhsm)
    })

    override def ge(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({
      lhsm.ge(rhsm)
    })

    override def lt(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({
      lhsm.lt(rhsm)
    })

    override def le(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({
      lhsm.le(rhsm)
    })

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

    override def none = MatrixM.none

    override def svd(m: MatrixMonT): SingularValueDecompositionT[MatrixMonT]#SvdResultT = m.svd

    override def qr(m: MatrixMonT): QRDecompositionT[MatrixMonT]#QRResultT = m.qr

    override def lu(m: MatrixMonT): LUDecompositionT[MatrixMonT]#LUResultT = m.lu

    override def cholesky(m: MatrixMonT): CholeskyDecompositionT[MatrixMonT]#CholeskyResultT = m.cholesky

  }

  implicit object Ev$MatrixExtensionsTC extends MatrixExtensionsTC[MatrixMonT]{
    override type MatrixDataElemT = ElemT //MatrixDataElemT

    override def acos(m: MatrixMonT): MatrixMonT = m.acos

    override def atan(m: MatrixMonT): MatrixMonT = m.atan

    override def log10(m: MatrixMonT): MatrixMonT = m.log10

    override def tanh(m: MatrixMonT): MatrixMonT = m.tanh

    override def log(m: MatrixMonT): MatrixMonT = m.log

    override def mapFunc(m: MatrixMonT, f: (MatrixDataElemT) => MatrixDataElemT): MatrixMonT = m.mapFunc(f)

    override def round(m: MatrixMonT): MatrixMonT = m.round

    override def cosh(m: MatrixMonT): MatrixMonT = m.cosh

    override def tan(m: MatrixMonT): MatrixMonT = m.tan

    override def cos(m: MatrixMonT): MatrixMonT = m.cos

    override def exp(m: MatrixMonT): MatrixMonT = m.exp

    override def foldFunc[W](m: MatrixMonT, w: W, f: (W, MatrixDataElemT) => W): Option[W] = m.foldFunc(w)(f)

    override def max(m: MatrixMonT): Option[MatrixDataElemT] = m.max

    override def expm1(m: MatrixMonT): MatrixMonT = m.expm1

    override def pow(m: MatrixMonT, p: MatrixDataElemT): MatrixMonT = m.pow(p)

    override def asinh(m: MatrixMonT): MatrixMonT = m.asinh

    override def asin(m: MatrixMonT): MatrixMonT = m.asin

    override def reduceFunc(m: MatrixMonT, v: MatrixDataElemT, f: (MatrixDataElemT, MatrixDataElemT) => MatrixDataElemT): Option[MatrixDataElemT] = m.reduceFunc(v)(f)

    override def floor(m: MatrixMonT): MatrixMonT = m.floor

    override def abs(m: MatrixMonT): MatrixMonT = m.abs

    override def min(m: MatrixMonT): Option[MatrixDataElemT] = m.min

    override def sqrt(m: MatrixMonT): MatrixMonT = m.sqrt

    override def reduceFilter(m: MatrixMonT, v: MatrixDataElemT, f: (MatrixDataElemT, Int, Int, MatrixDataElemT) => MatrixDataElemT): Option[MatrixDataElemT] = m.reduceFilter(v)(f)

    override def mapFilter(m: MatrixMonT, f: (Int, Int, MatrixDataElemT) => MatrixDataElemT): MatrixMonT = m.mapFilter(f)

    override def log1p(m: MatrixMonT): MatrixMonT = m.log1p

    override def sin(m: MatrixMonT): MatrixMonT = m.sin

    override def ceil(m: MatrixMonT): MatrixMonT = m.ceil

    override def atanh(m: MatrixMonT): MatrixMonT = m.atanh

    override def acosh(m: MatrixMonT): MatrixMonT = m.acosh

    override def sinh(m: MatrixMonT): MatrixMonT = m.sinh
  }


}

