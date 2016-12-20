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

import com.kabouterlabs.matrix.MatrixOperations.MatrixOperationsTC
import com.kabouterlabs.matrix._
import org.jblas.{ComplexDoubleMatrix, Eigen, Solve, DoubleMatrix}
import spire.math.Numeric
import scala.io.Source

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
  type MatrixImpl = DoubleMatrix
  type MatrixDouble = MatrixM[MatrixImpl]
  type jblasEigenResult = EigenResultM[(ComplexDoubleMatrix, Array[ComplexDoubleMatrix])]


  implicit class  JblassFormatterImplit$Ev(matrix:MatrixDouble) extends FormatterT {

    override def stringefy: String = matrix.matrix match {
      case Some(m) =>  "{" + m.getClass.getName + " " + matrix.rows + " * " + matrix.columns + "\n" +
        m.toString.replace("[", "[ ").replace("]", " ]").replace(";", " ]\n[") + "}"
      case None    => "{none}"
    }
  }

  implicit class JblassSizeImplicit$Ev(matrix:MatrixDouble) extends SizeT {
    override val rows: Int = matrix.map(_.rows)
    override val columns: Int = matrix.map(_.columns)
    override val size: Int = matrix.map(_.length)
    override val isNull: Boolean = matrix.matrix match {
      case Some(_) => false
      case None    => true
    }

  }

  //--------------------------------------------------------------------------------------------------------------
  //
  implicit val fdouble$ = new FactoryT {
    type MatrixImplT = MatrixImpl

    implicit override def apply(row: Int, col: Int, data: Array[Double]): Option[MatrixImplT] = JblasDoubleMatrix(row, col, data)

    implicit override def apply(row: Int, col: Int): Option[MatrixImplT] = JblasDoubleMatrix(row, col)

  }

  implicit object jblasDoubleMatrixCompanion extends CompanionT {
    override type MatrixImplT = MatrixDouble

    override def fill(row: Int, col: Int, value: ElemT): MatrixDouble = MatrixM(row, col, Array.fill[Double](row * col)(value))

    override def ones(row: Int, col: Int): MatrixDouble = MatrixM({DoubleMatrix.ones(row, col)})

    override def rand(row: Int, col: Int): MatrixImplT = MatrixM({DoubleMatrix.rand(row, col)})

    override def eye(size: Int): MatrixImplT = MatrixM({DoubleMatrix.eye(size)})

    override def zeros(row: Int, col: Int): MatrixImplT = MatrixM({DoubleMatrix.zeros(row, col)})

    //TODO check this
    override def diag(data: Array[Double]): MatrixImplT = {
      fdouble$(data.length, 1, data) match {
        case None => new MatrixDouble(None)
        case Some(a) => MatrixM.apply({DoubleMatrix.diag(a)})
      }
    }
  }


  implicit class AggregationT$implicit(matrix: MatrixDouble) extends AggregateT[MatrixDouble] {
    override def sumRows(): MatrixDouble =  for (m <- matrix) yield MatrixM({m.rowSums()})

    override def sumCols(): MatrixDouble =  for (m <- matrix) yield MatrixM({m.columnSums()})

    override def trace(): Option[ElemT] =   matrix.safeMap(_.diag.sum)

    override def sum(): Option[ElemT] = matrix.safeMap(_.sum())
  }

  implicit class SliceT$implicit(matrix: MatrixDouble) extends SliceT[MatrixDouble] {
    private def @#(matrix: MatrixDouble, a1: Int, a2: Int, a3: Int, a4: Int, f: (MatrixImpl, Int, Int, Int, Int) => MatrixImpl): MatrixDouble =
      for (m <- matrix) yield MatrixM({ f(m, a1, a2, a3, a4)})

    override def deepCopy: MatrixDouble = for (m <- matrix) yield MatrixM({m.dup()})

    override def apply(row: Int, coll: Int, v: ElemT): MatrixDouble =  matrix.map1((m: DoubleMatrix) => {m.put(row, coll, v); m})

    override def toDiag: MatrixDouble = for (m <- matrix) yield MatrixM({m.diag()})

    override def concatRight(rhs: MatrixDouble): MatrixDouble =   for (lhsm <- matrix; rhsm<-rhs) yield MatrixM({DoubleMatrix.concatHorizontally(lhsm, rhsm)})

    override def concatDown(rhs: MatrixDouble): MatrixDouble = for (lhsm <- matrix; rhsm<-rhs) yield   MatrixM({DoubleMatrix.concatVertically(lhsm, rhsm)})

    override def toArray: Option[Array[ElemT]] = matrix.safeMap(_.toArray)

    override def apply(row: Int, coll: Int): Option[Double] = matrix.safeMap(_.get(row,coll))

    override def apply[K, L](row: K, col: L): MatrixDouble = (row, col) match {
      case (r: Range, `::`) => @#(matrix, r.start, r.end, 0, 0, (m: DoubleMatrix, start: Int, end: Int, _, _) => m.getRange(start, end + 1, 0,m.columns))
      case (`::`, r: Range) => @#(matrix, r.start, r.end, 0, 0, (m: DoubleMatrix, start: Int, end: Int, _, _) => m.getRange(0,m.rows,start, end + 1))
      case (row: Int, `::`) => @#(matrix, row, 0, 0, 0, (m: DoubleMatrix, row: Int, _, _, _) => m.getRow(row))
      case (`::`, col: Int) => @#(matrix, col, 0, 0, 0, (m: DoubleMatrix, col: Int, _, _, _) => m.getColumn(col))
      case (r: Range, c: Range) => @#(matrix, r.start, r.end + 1, c.start, c.end + 1, (m: DoubleMatrix, rs, re, cs, ce) => m.getRange(rs, re, cs, ce))
      case (row: Int, r: Range) => @#(matrix, row, row + 1, r.start, r.end + 1, (m: DoubleMatrix, rs, re, cs, ce) => m.getRange(rs, re, cs, ce))
      case (r: Range, col: Int) => @#(matrix, r.start, r.end + 1, col, col + 1, (m: DoubleMatrix, rs, re, cs, ce) => m.getRange(rs, re, cs, ce))
      case (_, _) => matrix
    }
  }

  implicit class LinearAlgebraT$implicit(matrix: MatrixDouble) extends LinearAlgebraT {

    override type MatrixRetTypeT = MatrixDouble
    override type EigenResultT = jblasEigenResult

    override def inverse: MatrixRetTypeT = for (m <- matrix) yield MatrixM({Solve.pinv(m)})

    //TODO : can be optimized based on matrix type..
    override def solve(rhs: MatrixDouble): MatrixRetTypeT = for (lhsm <- matrix; rhsm<-rhs) yield MatrixM({Solve.solve(lhsm,rhsm)})

    override def eig: EigenResultT = for (m <- matrix) yield EigenResultM({(org.jblas.Eigen.eigenvalues(m), Eigen.eigenvectors(m))})

    override def transpose: MatrixRetTypeT = matrix.map1(_.transpose())

    override def determinant: Option[Double] = None //need eigen values

  }

  implicit object SerializeT$implicit extends SerializeT[MatrixDouble] {
    override def csvWrite(fn: String, matrix: MatrixDouble): Unit =   matrix.map(JblasDoubleMatrix.csvwrite(new File(fn), _))

    override def csvRead(fn: String): MatrixDouble = MatrixM({JblasDoubleMatrix.csvread(new File(fn))})
  }

  //TODO : Complex eigenvalues/eigenvectors aren't handled

  implicit class EigenAccessT$implicit(result: jblasEigenResult) extends EigenAccessT[MatrixDouble] {
    def name = "jblas result"

    override def vectors: MatrixDouble = MatrixM.none//MatrixM(result.result.map((r) => r.vectors.real().concatRight(r.vectors.imag())))

    override def values: MatrixDouble = MatrixM.none //MatrixM(result.result.map((r) => r.values.real().concatRight(r.values.imag())))

  }

  implicit object MatrixOperationsTC$implicit$ extends MatrixOperationsTC[MatrixDouble] {

    override def deepCopy(lhs: MatrixDouble): MatrixDouble = lhs.deepCopy

    override type EigenResultT = jblasEigenResult

    override def eig(m: MatrixDouble): EigenResultT = m.eig

    override def vectors(r: EigenResultT): MatrixDouble = r.vectors

    override def values(r: EigenResultT): MatrixDouble = r.values

    override def eigen(m: MatrixDouble): (MatrixDouble, MatrixDouble) = {val e = m.eig; (e.values, e.vectors)}

    override def add(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (lhsm <- lhs; rhsm<-rhs) yield {MatrixM({lhsm.add(rhsm)})}

    override def sub(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (lhsm <- lhs; rhsm<-rhs) yield {MatrixM({lhsm.sub(rhsm)})}

    override def mult(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (lhsm <- lhs; rhsm<-rhs) yield {MatrixM({lhsm.mmul(rhsm)})}

    override def multe(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (lhsm <- lhs; rhsm<-rhs) yield {MatrixM({lhsm.mul(rhsm)})}

    override def dive(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble =  for (lhsm <- lhs; rhsm<-rhs) yield {MatrixM({lhsm.div(rhsm)})}

    override def add1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble = for (lhsm<- lhs) yield MatrixM({JblasDoubleMatrix.add(lhsm,rhs)})

    override def sub1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble =   for (lhsm<- lhs) yield MatrixM({JblasDoubleMatrix.sub(lhsm,rhs)})

    override def mul1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble =   for (lhsm<- lhs) yield MatrixM({JblasDoubleMatrix.mul(lhsm,rhs)})

    override def div1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble =   for (lhsm<- lhs) yield MatrixM({JblasDoubleMatrix.div(lhsm,rhs)})

    override def eq(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble =  for (lhsm <- lhs; rhsm<-rhs) yield MatrixM({lhsm.eq(rhsm)})

    override def ne(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble =  for (lhsm <- lhs; rhsm<-rhs) yield MatrixM({lhsm.ne(rhsm)})

    override def gt(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (lhsm <- lhs; rhsm<-rhs) yield MatrixM({lhsm.gt(rhsm)})

    override def ge(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble =  for (lhsm <- lhs; rhsm<-rhs) yield MatrixM({lhsm.ge(rhsm)})

    override def lt(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (lhsm <- lhs; rhsm<-rhs) yield MatrixM({lhsm.lt(rhsm)})

    override def le(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = for (lhsm <- lhs; rhsm<-rhs) yield MatrixM({lhsm.le(rhsm)})

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

