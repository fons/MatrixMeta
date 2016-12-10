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

import breeze.linalg.{DenseVector, DenseMatrix}
import com.kabouterlabs.matrix.MatrixOperations
import spire.math.Numeric

/**
  * Created by fons on 4/13/16.
  */
object BreezeDenseMatrixInt {

  //  object BreezeDenseMatrixImplicit {
  //    type MatrixBoolean = Matrix[Boolean, Option[DenseMatrix[Boolean]]]
  //    type MatrixDouble = Matrix[Double, Option[DenseMatrix[Double]]]
  //    type MatrixInt = Matrix[Int, Option[DenseMatrix[Int]]]
  //    type BreezeEigenResult = EigenResult[Option[breeze.linalg.eig.DenseEig]]
  //
  //    implicit val fboolean = new BooleanFactory
  //    implicit val fdouble = new BreezeDenseMatrixFactory[Double]
  //    implicit val fint$ = new BreezeDenseMatrixFactory[Int]
  //
  //    implicit object MatrixDoubleOps$ extends MatrixOperationsTC[MatrixDouble] {
  //      type ElemT = Double
  //      type MaT = DenseMatrix[ElemT]
  //      type WraT = Matrix[ElemT, Option[DenseMatrix[ElemT]]]
  //
  //      private val gh = new MatrixOperator$[ElemT]
  //
  //      override def add(lhs: WraT, rhs: WraT): WraT = gh.ops1(lhs, rhs, (l: MaT, r: MaT) => l + r)
  //
  //      override def multe(lhs: WraT, rhs: WraT): WraT = gh.ops1(lhs, rhs, (l: MaT, r: MaT) => l :* r)
  //
  //      override def dive(lhs: WraT, rhs: WraT): WraT = gh.ops1(lhs, rhs, (l: MaT, r: MaT) => l :/ r)
  //
  //      override def mult(lhs: WraT, rhs: WraT): WraT = gh.ops1(lhs, rhs, (l: MaT, r: MaT) => l * r)
  //    }
  //
  //    implicit object MatrixIntOps$ extends MatrixOperationsTC[MatrixInt] {
  //      type ElemT = Int
  //
  //      type MaT = DenseMatrix[ElemT]
  //      type WraT = Matrix[ElemT, Option[DenseMatrix[ElemT]]]
  //
  //      private val gh = new MatrixOperator$[ElemT]
  //
  //      override def add(lhs: WraT, rhs: WraT): WraT = gh.ops1(lhs, rhs, (l: MaT, r: MaT) => l + r)
  //
  //      override def multe(lhs: WraT, rhs: WraT): WraT = gh.ops1(lhs, rhs, (l: MaT, r: MaT) => l :* r)
  //
  //      override def dive(lhs: WraT, rhs: WraT): WraT = gh.ops1(lhs, rhs, (l: MaT, r: MaT) => l :/ r)
  //
  //      override def mult(lhs: WraT, rhs: WraT): WraT = gh.ops1(lhs, rhs, (l: MaT, r: MaT) => l * r)
  //    }
  //
  //
  //    implicit object MatrixDoubleOpsDouble$ extends MatrixOpsByElementT[MatrixDouble] {
  //      type ElemT = Double
  //
  //      type MaT = DenseMatrix[ElemT]
  //      type WraT = Matrix[ElemT, Option[DenseMatrix[ElemT]]]
  //
  //      val gh = new MatrixOperator$[ElemT]
  //
  //      def add1[B: Numeric](lhs: WraT, rhs: B) = gh.ops2(lhs, rhs, (l: MaT, r: B) => BreezeDenseMatrix.adddouble(l.copy, r))
  //
  //      def subtract1[B: Numeric](lhs: WraT, rhs: B) = gh.ops2(lhs, rhs, (l: MaT, r: B) => BreezeDenseMatrix.subdouble(l.copy, r))
  //
  //      def multiply1[B: Numeric](lhs: WraT, rhs: B) = gh.ops2(lhs, rhs, (l: MaT, r: B) => BreezeDenseMatrix.muldouble(l.copy, r))
  //
  //      def divide1[B: Numeric](lhs: WraT, rhs: B) = gh.ops2(lhs, rhs, (l: MaT, r: B) => BreezeDenseMatrix.divdouble(l.copy, r))
  //    }
  //
  //    implicit object MatrixIntOpsDouble$ extends MatrixOpsByElementT[MatrixInt] {
  //      type ElemT = Int
  //
  //      type MaT = DenseMatrix[ElemT]
  //      type WraT = Matrix[ElemT, Option[DenseMatrix[ElemT]]]
  //
  //      val gh = new MatrixOperator$[ElemT]
  //
  //      def add1[B: Numeric](lhs: WraT, rhs: B) = gh.ops2(lhs, rhs, (l: MaT, r: B) => BreezeDenseMatrix.addint(l.copy, r))
  //
  //      def subtract1[B: Numeric](lhs: WraT, rhs: B) = gh.ops2(lhs, rhs, (l: MaT, r: B) => BreezeDenseMatrix.subint(l.copy, r))
  //
  //      def multiply1[B: Numeric](lhs: WraT, rhs: B) = gh.ops2(lhs, rhs, (l: MaT, r: B) => BreezeDenseMatrix.mulint(l.copy, r))
  //
  //      def divide1[B: Numeric](lhs: WraT, rhs: B) = gh.ops2(lhs, rhs, (l: MaT, r: B) => BreezeDenseMatrix.divint(l.copy, r))
  //    }
  //
  //
  //    implicit object MatrixLogicOpsByElementDouble$ extends MatrixCompareOpsByElementT[MatrixDouble, MatrixBoolean] {
  //      type ElemT = Double
  //
  //      type MaT = DenseMatrix[ElemT]
  //      type WraT = Matrix[ElemT, Option[DenseMatrix[ElemT]]]
  //      type RraT = Matrix[Boolean, Option[DenseMatrix[Boolean]]]
  //
  //      val gh = new MatrixOperator$[ElemT]
  //
  //      override def eq(lhs: WraT, rhs: WraT): RraT = gh.opsb(lhs, rhs, (l: MaT, r: MaT) => l :== r)
  //
  //      def ne(lhs: WraT, rhs: WraT) = gh.opsb(lhs, rhs, (l: MaT, r: MaT) => !(l :== r))
  //
  //      def lt(lhs: WraT, rhs: WraT) = gh.opsb(lhs, rhs, (l: MaT, r: MaT) => !(l :< r))
  //
  //      def le(lhs: WraT, rhs: WraT) = gh.opsb(lhs, rhs, (l: MaT, r: MaT) => (l :< r) :| (l :== r))
  //
  //      def ge(lhs: WraT, rhs: WraT) = gh.opsb(lhs, rhs, (l: MaT, r: MaT) => (!(l :< r)))
  //
  //      def gt(lhs: WraT, rhs: WraT) = gh.opsb(lhs, rhs, (l: MaT, r: MaT) => (!((l :< r) :| (l :== r))))
  //    }
  //
  //    implicit object MatrixLogicOpsByElementInt$ extends MatrixCompareOpsByElementT[MatrixInt, MatrixBoolean] {
  //      type ElemT = Int
  //
  //      type MaT = DenseMatrix[ElemT]
  //      type WraT = Matrix[ElemT, Option[DenseMatrix[ElemT]]]
  //      type RraT = Matrix[Boolean, Option[DenseMatrix[Boolean]]]
  //
  //      val gh = new MatrixOperator$[ElemT]
  //
  //      override def eq(lhs: WraT, rhs: WraT): RraT = gh.opsb(lhs, rhs, (l: MaT, r: MaT) => l :== r)
  //
  //      def ne(lhs: WraT, rhs: WraT) = gh.opsb(lhs, rhs, (l: MaT, r: MaT) => !(l :== r))
  //
  //      def lt(lhs: WraT, rhs: WraT) = gh.opsb(lhs, rhs, (l: MaT, r: MaT) => !(l :< r))
  //
  //      def le(lhs: WraT, rhs: WraT) = gh.opsb(lhs, rhs, (l: MaT, r: MaT) => (l :< r) :| (l :== r))
  //
  //      def ge(lhs: WraT, rhs: WraT) = gh.opsb(lhs, rhs, (l: MaT, r: MaT) => (!(l :< r)))
  //
  //      def gt(lhs: WraT, rhs: WraT) = gh.opsb(lhs, rhs, (l: MaT, r: MaT) => (!((l :< r) :| (l :== r))))
  //    }
  //
  //    implicit object BreezeDenseMatrixCompanion extends CompanionT[Double] {
  //      override type MatrixImplT = MatrixDouble
  //
  //      override def rand(row: Int, col: Int): MatrixDouble = Alloc2[Double](() => {
  //        DenseMatrix.rand[Double](row, col)
  //      })
  //
  //      override def eye(size: Int): MatrixDouble = Alloc2(() => DenseMatrix.eye[Double](size))
  //
  //      override def diag(data: Array[Double]): MatrixDouble = Alloc2(() => breeze.linalg.diag(new DenseVector[Double](data)))
  //
  //      override def zeros(row: Int, col: Int): MatrixDouble = Alloc2(() => DenseMatrix.zeros[Double](row, col))
  //    }
  //
  //    implicit object BreezeDenseMatrixCompanionInt extends CompanionT[Int] {
  //      override type MatrixImplT = MatrixInt
  //
  //      override def rand(row: Int, col: Int): MatrixInt = Alloc2(() => DenseMatrix.rand[Double](row, col).mapValues((1.0 / _ * 25.0)).mapValues(_.toInt))
  //
  //      override def eye(size: Int): MatrixInt = Alloc2(() => DenseMatrix.eye[Int](size))
  //
  //      override def zeros(row: Int, col: Int): MatrixInt = Alloc2(() => DenseMatrix.zeros[Int](row, col))
  //
  //      override def diag(data: Array[Int]): MatrixInt = Alloc2(() => breeze.linalg.diag(new DenseVector[Int](data)))
  //    }
  //
  //    implicit class AccessDouble(matrix: MatrixDouble) extends SliceT[Option[Double], MatrixDouble] {
  //      def name = "breeze double matrix" + matrix.toString
  //
  //      type ElemT = Double
  //      type MaT = DenseMatrix[ElemT]
  //      type WraT = Matrix[ElemT, Option[DenseMatrix[ElemT]]]
  //
  //      val gh = new MatrixOperator$[ElemT]
  //
  //      // breeze supports step 1 size only
  //      override def apply[K, L](row: K, col: L): WraT = (row, col) match {
  //        case (r: Range, ::) => gh.ops3(matrix, (m: MaT) => m(r.start to r.end, ::))
  //        case (::, r: Range) => gh.ops3(matrix, (m: MaT) => m(::, r.start to r.end))
  //
  //        case (row: Int, ::) => gh.ops3(matrix, (m: MaT) => m(row, ::).t.toDenseMatrix)
  //        case (::, col: Int) => gh.ops3(matrix, (m: MaT) => m(::, col).toDenseMatrix)
  //
  //        case (r: Range, c: Range) => gh.ops3(matrix, (m: MaT) => m(r.start to r.end, c.start to c.end))
  //        case (row: Int, r: Range) => gh.ops3(matrix, (m: MaT) => m(row, r.start to r.end).t.toDenseMatrix)
  //        case (r: Range, col: Int) => gh.ops3(matrix, (m: MaT) => m(r.start to r.end, col).toDenseMatrix)
  //        case (_, _) => matrix
  //      }
  //
  //      override def apply(row: Int, col: Int): Option[ElemT] = {
  //        matrix.matrix match {
  //          case Some(matrx) => Some(matrx(row, col))
  //          case None => None
  //        }
  //      }
  //    }
  //
  //    implicit class AccessInt(matrix: MatrixInt) extends SliceT[Option[Int], MatrixInt] {
  //      def name = "breeze integer matrix" + matrix.toString
  //
  //      type ElemT = Int
  //      type MaT = DenseMatrix[ElemT]
  //      type WraT = Matrix[ElemT, Option[DenseMatrix[ElemT]]]
  //
  //      val gh = new MatrixOperator$[ElemT]
  //
  //      // breeze supports step 1 size only
  //      override def apply[K, L](row: K, col: L): WraT = (row, col) match {
  //        case (r: Range, ::) => gh.ops3(matrix, (m: MaT) => m(r.start to r.end, ::))
  //        case (::, r: Range) => gh.ops3(matrix, (m: MaT) => m(::, r.start to r.end))
  //
  //        case (row: Int, ::) => gh.ops3(matrix, (m: MaT) => m(row, ::).t.toDenseMatrix)
  //        case (::, col: Int) => gh.ops3(matrix, (m: MaT) => m(::, col).toDenseMatrix)
  //
  //        case (r: Range, c: Range) => gh.ops3(matrix, (m: MaT) => m(r.start to r.end, c.start to c.end))
  //        case (row: Int, r: Range) => gh.ops3(matrix, (m: MaT) => m(row, r.start to r.end).t.toDenseMatrix)
  //        case (r: Range, col: Int) => gh.ops3(matrix, (m: MaT) => m(r.start to r.end, col).toDenseMatrix)
  //        case (_, _) => matrix
  //      }
  //
  //      override def apply(row: Int, col: Int): Option[ElemT] = {
  //        matrix.matrix match {
  //          case Some(matrx) => Some(matrx(row, col))
  //          case None => None
  //        }
  //      }
  //    }
  //
  //
  //    implicit class BreezeDenseMatrixDoubleLinearAlgebra$(matrix: MatrixDouble) extends LinearAlgebraT[Option[Double], MatrixDouble] {
  //
  //      type ElemT = Double
  //      type MaT = DenseMatrix[ElemT]
  //      type WraT = Matrix[ElemT, Option[DenseMatrix[ElemT]]]
  //      type RraT = WraT
  //
  //      override type MatrixRetTypeT = RraT
  //      override type EigenResultT = BreezeEigenResult
  //
  //      val gh = new MatrixOperator$[ElemT]
  //
  //      override def eig(): EigenResultT = {
  //        matrix.matrix match {
  //          case Some(m) => new BreezeEigenResult(Some(breeze.linalg.eig(m)))
  //          case None => new BreezeEigenResult(None)
  //        }
  //
  //      }
  //
  //      //new EigenResult[breeze.linalg.eig.DenseEig](breeze.linalg.eig(matrix.matrix))
  //
  //      override def solve(rhs: WraT): WraT = gh.ops1(matrix, rhs, (l: MaT, r: MaT) => l \ r)
  //
  //      override def inverse(): MatrixRetTypeT = gh.ops3(matrix, (m: MaT) => inv(m))
  //
  //      override def transpose(): WraT = gh.ops3(matrix, (m: MaT) => m.t)
  //
  //      override def determinant(): Option[ElemT] = {
  //        matrix.matrix match {
  //          case Some(matrx) => Some(det(matrx))
  //          case None => None
  //        }
  //      }
  //    }
  //
  //    implicit class BreezeDenseMatrixIntLinearAlgebra$(matrix: MatrixInt) extends LinearAlgebraT[Option[Int], MatrixInt] {
  //
  //      type ElemT = Int
  //      type MaT = DenseMatrix[ElemT]
  //      type WraT = Matrix[ElemT, Option[DenseMatrix[ElemT]]]
  //      type RraT = Matrix[Double, Option[DenseMatrix[Double]]]
  //
  //      override type MatrixRetTypeT = RraT
  //      override type EigenResultT = BreezeEigenResult
  //
  //      val gh = new MatrixOperator$[ElemT]
  //
  //      override def eig(): EigenResultT = {
  //        matrix.matrix match {
  //          case Some(m) => new BreezeEigenResult(Some(breeze.linalg.eig(m.mapValues(_.toDouble))))
  //          case None => new BreezeEigenResult(None)
  //        }
  //
  //      }
  //
  //      //new EigenResult[breeze.linalg.eig.DenseEig](breeze.linalg.eig(matrix.matrix))
  //
  //      override def solve(rhs: WraT): RraT = {
  //        (matrix.matrix, rhs.matrix) match {
  //          case (Some(lhs), Some(rhs)) => Alloc2(() => lhs.mapValues(_.toDouble) \ rhs.mapValues(_.toDouble))
  //          case (_, _) => new RraT(None)
  //        }
  //      }
  //
  //      override def inverse(): MatrixRetTypeT = {
  //        matrix.matrix match {
  //          case (Some(m)) => Alloc2(() => inv(m))
  //          case (_) => new MatrixRetTypeT(None)
  //        }
  //      }
  //
  //      override def transpose(): WraT = gh.ops3(matrix, (m: MaT) => m.t)
  //
  //      override def determinant(): Option[ElemT] = {
  //        matrix.matrix match {
  //          case Some(matrx) => Some(det(matrx).toInt)
  //          case None => None
  //        }
  //      }
  //    }
  //
  //    implicit class BreezeDenseDoubleEigenResult$(eresult: BreezeEigenResult) extends EgeinAccessT[MatrixDouble] {
  //      def name = "jeigen breeze dense matrix result"
  //
  //      override def values(): MatrixDouble = {
  //        eresult.result match {
  //          case None => new MatrixDouble(None)
  //          case Some(r) => Alloc2(() => DenseMatrix.vertcat(r.eigenvalues.toDenseMatrix, r.eigenvectorsComplex.toDenseMatrix).t)
  //        }
  //      }
  //
  //      override def vectors(): MatrixDouble = new MatrixDouble(None)
  //    }
  //
  //
  //  }


}
