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

package com.kabouterlabs.matrix.implicits.armadillojava

import java.io.{PrintWriter, StringWriter, File}

import com.kabouterlabs.matrix._

import com.kabouterlabs.matrix.MatrixOperations.MatrixOperationsTC


import org.armadillojava.{Col, Mat,Arma}


import spire.math.Numeric
import scala.io.Source

/**
  * Created by fons on 3/21/16.
  */


private object ArmadilloJavaDenseMatrix {

  def apply(rows: Int, colls: Int, data: Array[Double]): Option[Mat] = {
    try {
      val f = for (i <- Range(0, rows)) yield Range(i, rows * colls, rows).map(data(_)).toArray
      Some(new Mat(f.toArray))
    }
    catch {
      case e: Throwable =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
      }
    }


  def apply(rows: Int, colls: Int): Option[Mat] = {
    try {

      Some(org.armadillojava.Arma.zeros(rows, colls))
    }
    catch {
      case e: Throwable =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
    }
  }

  def csvwrite(file: File, matrix: Mat): Unit = {
    val pw = new PrintWriter(file)
    for (i <- Range(0, matrix.n_rows)) pw.write(matrix.row(i).memptr().mkString(",") ++ "\n")
    pw.close()
  }

  def csvread(file: File): Mat = {
    val l = (for (line <- Source.fromFile(file).getLines()) yield {
      line.split(",").map((s) => s.toDouble)
    }).toArray
    new Mat(l)
  }

  def add[B: Numeric](matrix: Mat, rhs: B): Mat = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.plus(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.plus(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.plus(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def sub[B: Numeric](matrix: Mat, rhs: B): Mat = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.minus(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.minus(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.minus(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def mul[B: Numeric](matrix: Mat, rhs: B): Mat = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.times(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.times(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.times(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def div[B: Numeric](matrix: Mat, rhs: B): Mat = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.divide(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.divide(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.divide(rhs.asInstanceOf[Int])
    case _ => matrix
  }


}

object ArmadilloJavaDenseMatrixImplicit {
  type ElemT         = Double
  type MatrixT   = Mat
  type MatrixMonT = MatrixM[MatrixT]
  //type ArmadilloEigenResult = EigenResultM[org.armadillojava.Col]
  type EigenResultT = org.armadillojava.Col

  private def @#(matrix: MatrixMonT, a1: Int, a2: Int, a3: Int, a4: Int, f: (MatrixT, Int, Int, Int, Int) => MatrixT): MatrixMonT =
    for (m <- matrix) yield MatrixM({ f(m, a1, a2, a3, a4)})


  //--------------------------------------------------------------------------------------------------------------
  //
  implicit class Ev$SizeT (matrix:MatrixMonT) extends SizeT {
    override val rows: Int = matrix.map(_.n_rows)
    override val columns: Int = matrix.map(_.n_cols)
    override val size: Int = matrix.map(_.size())
    override val isNull: Boolean = matrix.matrix match {
      case Some(_) => false
      case None    => true
    }
  }

  implicit class Ev$FormatterT(matrix:MatrixMonT) extends FormatterT {
    override def stringefy = matrix.matrix match {
      case Some(m) =>  "{" + m.getClass.getName + "\n" + m.toString + "}"
      case None    => "{none}"
    }

  }


  implicit val ev$FactoryT = new FactoryT {
    type MatrixImplT = MatrixT

    implicit override def apply(row: Int, col: Int, data: Array[Double]): Option[MatrixImplT] = ArmadilloJavaDenseMatrix(row, col, data)

    implicit override def apply(row: Int, col: Int): Option[MatrixImplT] = ArmadilloJavaDenseMatrix(row, col)

  }

  implicit object Ev$CompanionT extends CompanionT {
    override type MatrixImplT = MatrixMonT

    override def fill(row: Int, col: Int, value: ElemT): MatrixMonT = MatrixM(row, col, Array.fill[Double](row * col)(value))

    override def ones(row: Int, col: Int): MatrixMonT = MatrixM.apply({ org.armadillojava.Arma.ones(row, col)})

    override def rand(row: Int, col: Int): MatrixImplT = MatrixM.apply({ org.armadillojava.Arma.randu(row, col)})

    override def eye(size: Int): MatrixImplT = MatrixM.apply({ org.armadillojava.Arma.eye(size, size)})

    override def zeros(row: Int, col: Int): MatrixImplT = MatrixM.apply({ org.armadillojava.Arma.zeros(row, col)})

    //TODO check this
    override def diag(data: Array[Double]): MatrixImplT = {
      ev$FactoryT(data.length, 1, data) match {
        case None => new MatrixMonT(None)
        case Some(a) => MatrixM.apply({ org.armadillojava.Arma.diagmat(new Col(a))})
      }
    }
  }


  implicit class Ev$AggregationT(matrix: MatrixMonT) extends AggregateT[MatrixMonT] {
    override def sumRows: MatrixMonT = matrix.map1((m: Mat) => org.armadillojava.Arma.cumsum(m, 0).rows(m.n_rows - 1, m.n_rows - 1))

    override def sumCols: MatrixMonT = matrix.map1((m: Mat) => org.armadillojava.Arma.cumsum(m, 1).cols(m.n_cols - 1, m.n_cols - 1))

    override def trace = matrix.safeMap(org.armadillojava.Arma.trace)

    override def sum: Option[ElemT] = matrix.safeMap(org.armadillojava.Arma.accu(_))

  }

  implicit class Ev$SliceT(matrix: MatrixMonT) extends SliceT[MatrixMonT] {
    private def @#(matrix: MatrixMonT, a1: Int, a2: Int, a3: Int, a4: Int, f: (MatrixT, Int, Int, Int, Int) => MatrixT): MatrixMonT =
      for (m <- matrix) yield MatrixM({ f(m, a1, a2, a3, a4)})

    override def deepCopy: MatrixMonT =  for (m <- matrix) yield MatrixM(m.n_rows, m.n_cols,m.memptr())

    override def apply(row: Int, coll: Int, v: ElemT): MatrixMonT = matrix.map1((m: Mat) => {m.at(row, coll, org.armadillojava.Op.EQUAL, v); m})

    override def toDiag: MatrixMonT = matrix.map1((m: Mat) => org.armadillojava.Arma.diagmat(m))

    override def concatRight(rhs: MatrixMonT): MatrixMonT = for(l<-matrix; r<- rhs) yield  MatrixM({org.armadillojava.Arma.join_horiz(l, r)})

    override def concatDown(rhs: MatrixMonT): MatrixMonT =  for(l<-matrix; r<- rhs) yield  MatrixM({org.armadillojava.Arma.join_vert(l, r)})

    override def toArray: Option[Array[ElemT]] = matrix.safeMap(_.memptr())

    override def apply(row: Int, coll: Int): Option[Double] = matrix.safeMap(_.at(row,coll))

    override def apply[K, L](row: K, col: L): MatrixMonT = (row, col) match {
      case (r: Range, `::`) => @#(matrix, r.start, r.end, 0, 0, (m: Mat, start: Int, end: Int, _, _) => m.rows(start, end))
      case (`::`, r: Range) => @#(matrix, r.start, r.end, 0, 0, (m: Mat, start: Int, end: Int, _, _) => m.cols(start, end))
      case (row: Int, `::`) => @#(matrix, row, 0, 0, 0, (m: Mat, row: Int, _, _, _) => m.rows(row, row))
      case (`::`, col: Int) => @#(matrix, col, 0, 0, 0, (m: Mat, col: Int, _, _, _) => m.cols(col, col))
      case (r: Range, c: Range) => @#(matrix, r.start, r.end + 1, c.start, c.end + 1, (m: Mat, rs, re, cs, ce) => m.submat(rs, re, cs, ce))
      case (row: Int, r: Range) => @#(matrix, row, row + 1, r.start, r.end + 1, (m: Mat, rs, re, cs, ce) => m.submat(rs, re, cs, ce))
      case (r: Range, col: Int) => @#(matrix, r.start, r.end + 1, col, col + 1, (m: Mat, rs, re, cs, ce) => m.submat(rs, re, cs, ce))
      case (_, _) => matrix
    }
  }

  class EigenAccess extends EigenAccessT[EigenResultT] {
    override def name = "apache commons result"
    type EigenVectorRetT = Option[EigenAccessT[EigenResultT]#EigenVectorT]
    type EigenValueRetT  = Option[EigenAccessT[EigenResultT]#EigenValuesT]
    override def vectors(result:Option[EigenResultT]):EigenVectorRetT  = None
    //MatrixM(result.result.map((r) => r.vectors.real().concatRight(r.vectors.imag())))

    override def values(result:Option[EigenResultT]):EigenValueRetT = None
    //MatrixM(result.result.map((r) => r.values.real().concatRight(r.values.imag())))

  }
  implicit class LinearAlgebraT$implicit(matrix: MatrixMonT) extends LinearAlgebraT {
    implicit val access = new EigenAccess
    override type MatrixRetTypeT = MatrixMonT
    override type EigenResultRetTypeT = EigenResultM[EigenResultT]

    override def inverse: MatrixRetTypeT = for (m<-matrix) yield MatrixM({m.i()})

    //TODO : can be optimized based on matrix type..
    override def solve(rhs: MatrixMonT): MatrixRetTypeT = for(l<-matrix; r<-rhs) yield MatrixM({org.armadillojava.Arma.solve(l, r)})

    override def eig: EigenResultRetTypeT = for (m <- matrix) yield EigenResultM({Arma.eig_sym(m)})

    override def transpose: MatrixRetTypeT = matrix.map1((m: Mat) => m.t)

    override def determinant: Option[Double] = matrix.safeMap(org.armadillojava.Arma.det)

  }

  implicit object Ev$SerializeT extends SerializeT[MatrixMonT] {
    override def csvWrite(fn: String, matrix: MatrixMonT): Unit =  matrix.map(ArmadilloJavaDenseMatrix.csvwrite(new File(fn), _))
    override def csvRead(fn: String): MatrixMonT = MatrixM({ ArmadilloJavaDenseMatrix.csvread(new File(fn))})
  }



  implicit object Ev$MatrixOperationsTC extends MatrixOperationsTC[MatrixMonT] {

    override def deepCopy(lhs: MatrixMonT): MatrixMonT = lhs.deepCopy

    override type EigenResultRetTypeT = EigenResultM[EigenResultT]

    override def eig(m: MatrixMonT): EigenResultRetTypeT = m.eig

    override def vectors(r: EigenResultM[EigenResultT]): Option[EigenResultRetTypeT#EigenVectorT] = r.vectors

    override def values(r: EigenResultM[EigenResultT]): Option[EigenResultRetTypeT#EigenValuesT] = r.values

    override def eigen(m: MatrixMonT): (Option[EigenResultRetTypeT#EigenValuesT]  ,Option[EigenResultRetTypeT#EigenVectorT] ) = {val e = m.eig; (e.values, e.vectors)}

    override def add(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({lhsm.plus(rhsm)})

    override def sub(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({lhsm.minus(rhsm)})

    override def mult(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({lhsm.times(rhsm)})

    override def multe(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({lhsm.elemTimes(rhsm)})

    override def dive(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT =for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({lhsm.elemDivide(rhsm)})

    override def add1[B: Numeric](lhs: MatrixMonT, rhs: B): MatrixMonT = for (lhsm<- lhs) yield MatrixM({ArmadilloJavaDenseMatrix.add(lhsm,rhs)})

    override def sub1[B: Numeric](lhs: MatrixMonT, rhs: B): MatrixMonT = for (lhsm<- lhs) yield MatrixM({ArmadilloJavaDenseMatrix.sub(lhsm,rhs)})

    override def mul1[B: Numeric](lhs: MatrixMonT, rhs: B): MatrixMonT = for (lhsm<- lhs) yield MatrixM({ArmadilloJavaDenseMatrix.mul(lhsm,rhs)})

    override def div1[B: Numeric](lhs: MatrixMonT, rhs: B): MatrixMonT = for (lhsm<- lhs) yield MatrixM({ArmadilloJavaDenseMatrix.div(lhsm,rhs)})

    override def eq(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({lhsm.equals(rhsm)})

    override def ne(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({lhsm.nonEquals(rhsm)})

    override def gt(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({lhsm.strictGreaterThan(rhsm)})

    override def ge(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({lhsm.greaterThan(rhsm)})

    override def lt(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({lhsm.strictLessThan(rhsm)})

    override def le(lhs: MatrixMonT, rhs: MatrixMonT): MatrixMonT = for (lhsm <- lhs; rhsm <- rhs) yield MatrixM({lhsm.lessThan(rhsm)})

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

