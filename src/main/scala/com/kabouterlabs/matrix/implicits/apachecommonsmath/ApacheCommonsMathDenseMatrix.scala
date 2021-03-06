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

import com.kabouterlabs.matrix.MatrixExtension.MatrixExtensionsTC
import com.kabouterlabs.matrix._

import com.kabouterlabs.matrix.MatrixOperations.MatrixOperationsTC
import com.kabouterlabs.matrix.except.HandleException


import org.apache.commons.math3.linear._


import spire.math.{Complex, Numeric}
import scala.io.Source
import scala.util.Random

import com.kabouterlabs.matrix.implicits.extension.MatrixExtensionImplicit.MatrixMapper._

/*
The array is column major order but the apache lib expects row major order.
 */
private object ApacheCommonsMathDenseMatrix {

  def apply(rows: Int, colls: Int, f: => Array[Double]): Option[RealMatrix] = HandleException{
    val a = (for (i <- Range(0, rows)) yield Range(i, rows * colls, rows).map(f(_)).toArray).toArray
    MatrixUtils.createRealMatrix(a)
  }

  def apply(rows: Int, colls: Int): Option[RealMatrix] = HandleException{MatrixUtils.createRealMatrix(rows, colls)}

  def lusolver(m: RealMatrix): Option[LUDecomposition] = {

    try {
      if (m.getRowDimension == m.getColumnDimension) Some(new LUDecomposition(m)) else None
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
      if (m.getRowDimension == m.getColumnDimension) Some(new EigenDecomposition(m)) else None
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
  type ElemT        = Double
  type MatrixT   = RealMatrix
  type MatrixMonT = MatrixM[MatrixT]
  type EigenResultT = org.apache.commons.math3.linear.EigenDecomposition //(RealMatrix, RealMatrix)

  private def @#(matrix: MatrixMonT, a1: Int, a2: Int, a3: Int, a4: Int, f: (MatrixT, Int, Int, Int, Int) => MatrixT): MatrixMonT =
    for (m <- matrix) yield MatrixM({
      f(m, a1, a2, a3, a4)
    })

  //--------------------------------------------------------------------------------------------------------------
  //
  implicit class Ev$SizeT (matrix:MatrixMonT) extends SizeT{
    override val rows: Int = matrix.map(_.getRowDimension)
    override val columns: Int = matrix.map(_.getColumnDimension)
    override val size: Int = rows * columns
    override val isNull: Boolean = matrix.matrix match {
      case Some(_) => false
      case None    => true
    }
  }

  implicit class Ev$FormatterT(matrix:MatrixMonT) extends FormatterT {
    override def stringefy = matrix.matrix match {
      case Some(m) =>  "{" + m.getClass.getName + "  " + matrix.rows + " * " + matrix.columns + "\n" + m.toString.replace("},{", "}\n{").replace(",", " , ").replace("Matrix{{", "Matrix{\n{") + "}"
      case None    => "{none}"
    }

  }

  implicit val ev$FactoryT = new FactoryT {
    type MatrixImplT = MatrixT

    implicit override def apply(row: Int, col: Int, data: Array[Double]): Option[MatrixImplT] = ApacheCommonsMathDenseMatrix(row, col, data)

    implicit override def apply(row: Int, col: Int): Option[MatrixImplT] = ApacheCommonsMathDenseMatrix(row, col)

  }

  implicit object Ev$CompanionT extends CompanionT {
    override type MatrixImplT = MatrixMonT

    override def fill(row: Int, col: Int, value: ElemT): MatrixMonT = MatrixM({
      ApacheCommonsMathDenseMatrix(row, col, {
        Array.fill[Double](row * col)(value)
      })
    })

    override def ones(row: Int, col: Int): MatrixMonT = fill(row, col, 1.0)

    override def rand(row: Int, col: Int): MatrixImplT = MatrixM({
      ApacheCommonsMathDenseMatrix(row, col, {
        Array.fill[Double](row * col)(Random.nextDouble())
      })
    })


    override def eye(size: Int): MatrixImplT = MatrixM({
      MatrixUtils.createRealIdentityMatrix(size)
    })

    override def zeros(row: Int, col: Int): MatrixImplT = fill(row, col, 0.0)

    override def diag(data: Array[Double]): MatrixImplT = {
      ev$FactoryT(data.length, 1, data) match {
        case None => MatrixM.none
        case Some(a) => MatrixM({
          MatrixUtils.createRealDiagonalMatrix(data)
        })
      }
    }
  }


  implicit class Ev$AggregationT(matrix: MatrixMonT) extends AggregateT[MatrixMonT] {
    override def sumRows: MatrixMonT = for (m <- matrix) yield
      MatrixM(1, m.getColumnDimension, {(for (i <- Range(0, m.getColumnDimension)) yield m.getColumn(i).sum).toArray})

    override def sumCols: MatrixMonT = for (m <- matrix) yield
      MatrixM(m.getRowDimension,1,{(for (i <- Range(0, m.getRowDimension)) yield m.getRow(i).sum).toArray })

    override def trace = matrix.safeMap(_.getTrace)

    override def sum: Option[ElemT] = matrix.safeMap(_.getData.flatten.sum)

  }

  implicit class Ev$SliceT(matrix: MatrixMonT) extends SliceT[MatrixMonT] {

    override def deepCopy: MatrixMonT = for (m <- matrix) yield MatrixM(m.getRowDimension, m.getColumnDimension, m.getData.transpose.flatten)

    override def apply(row: Int, coll: Int, v: ElemT): MatrixMonT = matrix.map1((m: RealMatrix) => {
      m.setEntry(row, coll, v); m
    })

    override def toDiag: MatrixMonT = for (m <- matrix) yield MatrixM({
      MatrixUtils.createRealDiagonalMatrix(Range(0, m.getRowDimension).zip(Range(0, m.getColumnDimension)).map(f => m.getEntry(f._1, f._2)).toArray)
    })

    override def concatRight(rhs: MatrixMonT): MatrixMonT = for (l <- matrix; r <- rhs) yield {
      MatrixM({
        for (m <- ApacheCommonsMathDenseMatrix(l.getRowDimension, l.getColumnDimension + r.getColumnDimension)) yield {
          m.setSubMatrix(l.getData, 0, 0);
          m.setSubMatrix(r.getData, 0, l.getColumnDimension)
          m;
        }
      })
    }

    override def concatDown(rhs: MatrixMonT): MatrixMonT = for (l <- matrix; r <- rhs) yield {
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

    override def apply[K, L](row: K, col: L): MatrixMonT = (row, col) match {
      case (r: Range, `::`)     => for (m <- matrix) yield MatrixM({m.getSubMatrix(r.start, r.end, 0, m.getColumnDimension - 1)})
      case (`::`, r: Range)     => for (m <- matrix) yield MatrixM({m.getSubMatrix(0, m.getRowDimension - 1, r.start, r.end)})
      case (row: Int, `::`)     => for (m <- matrix) yield MatrixM({m.getSubMatrix(row, row, 0, m.getColumnDimension - 1)})
      case (`::`, col: Int)     => for (m <- matrix) yield MatrixM({m.getSubMatrix(0, m.getRowDimension - 1, col, col)})
      case (r: Range, c: Range) => for (m <- matrix) yield MatrixM({m.getSubMatrix(r.start, r.end, c.start, c.end)})
      case (row: Int, c: Range) => for (m <- matrix) yield MatrixM({m.getSubMatrix(row, row, c.start, c.end)})
      case (r: Range, col: Int) => for (m <- matrix) yield MatrixM({m.getSubMatrix(r.start, r.end, col, col)})

      case (_, _) => matrix
    }
  }

  class EigenAccess extends EigenAccessT[EigenResultT] {
    override def name = "apache commons result"
    type EigenVectorRetT = Option[EigenAccessT[EigenResultT]#EigenVectorT]
    type EigenValueRetT  = Option[EigenAccessT[EigenResultT]#EigenValuesT]
    override def vectors(result:Option[EigenResultT]):EigenVectorRetT  = for (e <- result) yield {
      (for (i <- Range(0, e.getImagEigenvalues.length, 1)) yield {
        if (e.getImagEigenvalue(i) == 0.0) {
          (for (r <- e.getEigenvector(i).toArray) yield Complex(r, 0.0)).toVector
        }
        else {
          if ((i % 2) == 0) {
            val rval = e.getEigenvector(i).toArray.zip(e.getEigenvector(i+1).toArray)
            (for ((real, imag) <- rval) yield {
              Complex(real, imag)
            }).toVector
          }
          else {
            val rval = e.getEigenvector(i-1).toArray.zip(e.getEigenvector(i).toArray)
            (for ((real, imag) <- rval) yield {
              Complex(real, -1.0 * imag)
            }).toVector
          }
        }

      }).toArray
    }


    override def values(result:Option[EigenResultT]):EigenValueRetT = for ( eigs<-result) yield {
      val eval = eigs.getD
      val blocks = eval.getColumnDimension
      for (i <- Range(0, eval.getColumnDimension, 1)) yield {
        if ( (i % 2) == 0) Complex(eval.getEntry(i,i),  if (i + 1 < blocks) eval.getEntry(i, i+1) else 0.0)
        else  Complex(eval.getEntry(i,i),  eval.getEntry(i, i-1))
      }
    }.toArray
  }

  implicit class Ev$LinearAlgebraT(matrix: MatrixMonT) extends LinearAlgebraT {
    implicit val access = new EigenAccess
    private lazy val lu = for (m <- matrix) yield ApacheCommonsMathDenseMatrix.lusolver(m)
    private lazy val qr = for (m <- matrix) yield ApacheCommonsMathDenseMatrix.qrsolver(m)
    private lazy val ei = for (m <- matrix) yield ApacheCommonsMathDenseMatrix.eigensolver(m)

    override type MatrixRetTypeT = MatrixMonT
    override type EigenResultRetTypeT = EigenResultM[EigenResultT]

    override def inverse: MatrixRetTypeT = MatrixM({
      matrix.safeMap((m) => qr.map(_.getSolver.getInverse)).flatten
    })

    //TODO : can be optimized based on matrix type..
    override def solve(rhs: MatrixMonT): MatrixRetTypeT = for (l <- matrix; r <- rhs) yield MatrixM({
      lu.map(_.getSolver.solve(r))
    })

    override def eig: EigenResultRetTypeT = EigenResultM({for (e <- ei) yield e})

    override def transpose: MatrixRetTypeT = matrix.map1((m: RealMatrix) => m.transpose())

    override def determinant: Option[Double] = lu.map(_.getDeterminant)

  }

  implicit object Ev$SerializeT extends SerializeT[MatrixMonT] {
    override def csvWrite(fn: String, matrix: MatrixMonT): Unit = matrix.map(ApacheCommonsMathDenseMatrix.csvwrite(new File(fn), _))

    override def csvRead(fn: String): MatrixMonT = MatrixM({
      ApacheCommonsMathDenseMatrix.csvread(new File(fn))
    })
  }

  implicit class Ev$SingularValueDecomposition(matrix:MatrixMonT) extends SingularValueDecompositionT[MatrixMonT] {
    override type SvdElemT = ElemT

    private case class SVD(U:MatrixMonT, Vt:MatrixMonT, S: Option[Array[ElemT]], Sm:MatrixMonT)

    private val svd_ = matrix.matrix match {
      case None => SVD(MatrixM.none, MatrixM.none, None, MatrixM.none)
      case Some(m) => {
        val svdr = new SingularValueDecomposition(m)
        SVD(MatrixM({svdr.getU}), MatrixM({svdr.getVT}), Some(svdr.getSingularValues), MatrixM({svdr.getS}))
      }
    }

    val svd = new SvdResultT {

      override lazy val toString: String = super.toString + "\n" + "S=" + svd_.S.map(_.mkString(",")) + "\n" + svd_.U.stringefy + "\n" + svd_.Vt.stringefy

      override val U: MatrixMonT           =  svd_.U
      override val S: Option[Array[ElemT]] =  svd_.S
      override val Vt: MatrixMonT          =  svd_.Vt
      override def Sm(): MatrixMonT        =  svd_.Sm
    }
  }

  implicit class Ev$QRDecompostion (matrix: MatrixMonT) extends QRDecompositionT[MatrixMonT] {
    private case class QR(q:MatrixMonT, r:MatrixMonT)
    private val qr_ = matrix.matrix match {
      case None => QR(MatrixM.none, MatrixM.none)
      case Some(m) => ApacheCommonsMathDenseMatrix.qrsolver(m) match {
        case None =>  QR(MatrixM.none, MatrixM.none)
        case Some(qrs) =>  QR(MatrixM({qrs.getQ}), MatrixM({qrs.getR}))
        }
      }

    val qr = new QRResultT {

      override lazy val toString: String = super.toString + "@" + super.hashCode() + "\n" + qr_.r.stringefy + "\n" + qr_.q.stringefy

      override val R: MatrixMonT = qr_.r
      override val Q: MatrixMonT = qr_.q
    }

  }

  implicit class Ev$LUDecompostion (matrix: MatrixMonT) extends LUDecompositionT[MatrixMonT] {

    private case class LU(upper:MatrixMonT, lower:MatrixMonT, perms : Option[Array[Int]], p:MatrixMonT)

    private val lu_ = matrix.matrix match {
      case None => LU(MatrixM.none, MatrixM.none, None, MatrixM.none)
      case Some(m) => ApacheCommonsMathDenseMatrix.lusolver(m) match {
        case None   =>   LU(MatrixM.none, MatrixM.none, None, MatrixM.none)
        case Some(lus) => {
          LU(MatrixM({lus.getU}), MatrixM({lus.getL}), Some(lus.getPivot), MatrixM({lus.getP}).inverse)
        }
      }
    }
    val lu = new LUResultT {
      override lazy val toString: String = super.toString + "@"+ hashCode() + "\n" + lu_.lower.stringefy + "\n" + lu_.upper.stringefy
      override val P: MatrixMonT =  lu_.p
      override val L: MatrixMonT =  lu_.lower
      override val U: MatrixMonT =  lu_.upper
      override val permutations: Option[Array[Int]] = lu_.perms
      override def constructP(perms: Option[Array[Int]]): MatrixMonT = lu_.p
    }

  }

  implicit class Ev$CholeskyDecompositionT(matrix:MatrixMonT) extends CholeskyDecompositionT[MatrixMonT]{
    private case class Cholesky(chol:MatrixMonT)
    private val cholesky_ = matrix.matrix match {
      case None    => Cholesky(MatrixM.none)
      case Some(m) => {
        Cholesky(MatrixM({new org.apache.commons.math3.linear.CholeskyDecomposition(m).getL}))
      }
    }

    val cholesky = new CholeskyResultT {
      override lazy val toString: String = super.toString + "@"+ hashCode() + "\n" + cholesky_.chol.stringefy
      override val L: MatrixMonT = cholesky_.chol
    }
  }


  implicit object Ev$MatrixOperationsTC extends MatrixOperationsTC[MatrixMonT] {



    override type MatrixDataElemT = Double



    private def op(lhs: MatrixMonT, rhs: MatrixMonT, f: (Double, Double) => Double): MatrixMonT =
      for (l <- lhs; r <- rhs) yield {
        val la = l.transpose.getData.flatten
        val lb = r.transpose.getData.flatten
        MatrixM({
          ApacheCommonsMathDenseMatrix(l.getRowDimension, l.getColumnDimension,
            la.zip(lb).map(e => f(e._1, e._2)))
        })
      }


    private def opb(lhs: MatrixMonT, rhs: MatrixMonT, f: (Double, Double) => Boolean): MatrixMonT =
      for (l <- lhs; r <- rhs) yield {
        val la = l.transpose.getData.flatten
        val lb = r.transpose.getData.flatten
        MatrixM({
          ApacheCommonsMathDenseMatrix(l.getRowDimension, l.getColumnDimension,
            la.zip(lb).map(e => if (f(e._1, e._2)) 1.0 else 0.0))
        })
      }



    override def rows(m: MatrixMonT): Int = m.rows

    override def columns(m: MatrixMonT): Int = m.columns

    override def size(m: MatrixMonT): Int = m.size

    override def isNull(m: MatrixMonT): Boolean = m.isNull

    override def deepCopy(lhs: MatrixMonT): MatrixMonT = lhs.deepCopy

    override type EigenResultRetTypeT =  EigenResultM[EigenResultT]

    override def eig(m: MatrixMonT): EigenResultRetTypeT = m.eig

    override def vectors(r: EigenResultM[EigenResultT]): Option[EigenResultRetTypeT#EigenVectorT] = r.vectors

    override def values(r: EigenResultM[EigenResultT]): Option[EigenResultRetTypeT#EigenValuesT] = r.values

    override def eigen(m: MatrixMonT): (Option[EigenResultRetTypeT#EigenValuesT]  ,Option[EigenResultRetTypeT#EigenVectorT] ) = {val e = m.eig; (e.values, e.vectors)}

    override def add(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({
      lhsm.add(rhsm)
    })

    override def sub(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({
      lhsm.subtract(rhsm)
    })

    override def mult(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({
      lhsm.multiply(rhsm)
    })

    override def multe(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = op(lhs, rhs, _ * _)

    override def dive(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = op(lhs, rhs, _ / _)


    override def add1[B: Numeric](lhs: MatrixMonT, rhs: B): MatrixMonT = for (lhsm <- lhs) yield MatrixM({
      ApacheCommonsMathDenseMatrix.add(lhsm, rhs)
    })

    override def sub1[B: Numeric](lhs: MatrixMonT, rhs: B): MatrixMonT = for (lhsm <- lhs) yield MatrixM({
      ApacheCommonsMathDenseMatrix.sub(lhsm, rhs)
    })

    override def mul1[B: Numeric](lhs: MatrixMonT, rhs: B): MatrixMonT = for (lhsm <- lhs) yield MatrixM({
      ApacheCommonsMathDenseMatrix.mul(lhsm, rhs)
    })

    override def div1[B: Numeric](lhs: MatrixMonT, rhs: B): MatrixMonT = for (lhsm <- lhs) yield MatrixM({
      ApacheCommonsMathDenseMatrix.div(lhsm, rhs)
    })

    override def eq(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = opb(lhs, rhs, _ == _)

    override def ne(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = opb(lhs, rhs, _ != _)

    override def gt(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = opb(lhs, rhs, _ > _)

    override def ge(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = opb(lhs, rhs, _ >= _)

    override def lt(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = opb(lhs, rhs, _ < _)

    override def le(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = opb(lhs, rhs, _ <= _)


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

    override def svd(m: MatrixMonT) = m.svd

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

