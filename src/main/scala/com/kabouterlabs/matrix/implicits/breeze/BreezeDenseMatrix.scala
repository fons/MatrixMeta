package com.kabouterlabs.matrix.implicits.breeze

import java.io.{File, PrintWriter, StringWriter}


import breeze.linalg.{DenseMatrix, DenseVector, det, inv, *, sum}
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
      case e:Throwable => {
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
      }
    }
  }

  def apply(rows: Int, cols: Int): Option[DenseMatrix[Double]] = {
    try {
      Some(DenseMatrix.zeros[Double](rows,cols))
    }
    catch {
      case e:Throwable => {
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
      }
    }
  }

  def adddouble[B: Numeric](matrix: DenseMatrix[Double], rhs: B): DenseMatrix[Double] = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix :+= rhs.asInstanceOf[Double]
    case q if rhs.isInstanceOf[Float] => matrix :+= rhs.asInstanceOf[Float].toDouble
    case q if rhs.isInstanceOf[Int]  => matrix :+= rhs.asInstanceOf[Int].toDouble
    case _ => matrix
  }


  def subdouble[B: Numeric](matrix: DenseMatrix[Double], rhs: B): DenseMatrix[Double] = rhs match {
    case q if rhs.isInstanceOf[Double]  => matrix :+= (-1.0 * rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix :+= (-1.0 * rhs.asInstanceOf[Float].toDouble)
    case q if rhs.isInstanceOf[Int]  => matrix :+= (-1.0 * rhs.asInstanceOf[Int].toDouble)
    case _ => matrix
  }

  def muldouble[B: Numeric](matrix: DenseMatrix[Double], rhs: B): DenseMatrix[Double] = rhs match {
    case q if rhs.isInstanceOf[Double]  => matrix :*= rhs.asInstanceOf[Double]
    case q if rhs.isInstanceOf[Float] => matrix :*= rhs.asInstanceOf[Float].toDouble
    case q if rhs.isInstanceOf[Int] => matrix :*= rhs.asInstanceOf[Int].toDouble
    case _ => matrix
  }

  def divdouble[B: Numeric](matrix: DenseMatrix[Double], rhs: B): DenseMatrix[Double] = rhs match {
    case q if rhs.isInstanceOf[Double]  => matrix :*= (1.0 / rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float]   => matrix :*= (1.0 / rhs.asInstanceOf[Float].toDouble)
    case q if rhs.isInstanceOf[Int]     => matrix :*= (1.0 / rhs.asInstanceOf[Int].toDouble)
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

  private def ~->[U](matrix:MatrixDouble, f:(MatrixImpl)=>U) : Option[U] = {
    matrix.matrix match {
      case None          => None
      case Some(matrixm) => {
        try {
          Some(f(matrixm))
        }
        catch {
          case e:Throwable => {
            val sw = new StringWriter
            e.printStackTrace(new PrintWriter(sw))
            println("exception caught :" + e + sw)
            None
          }
        }

      }
    }
  }


  private def ~>(lhs:MatrixDouble, rhs:MatrixDouble, f:(MatrixImpl, MatrixImpl)=>MatrixImpl):MatrixDouble = {
    (lhs.matrix, rhs.matrix) match {
      case (Some(lhsm: MatrixImpl), Some(rhsm: MatrixImpl)) => MatrixM(()=>f(lhsm, rhsm))
      case (_, _) => MatrixM.none
    }
  }
  private def :~>[B](lhs:MatrixDouble, rhs:B, f:(MatrixImpl, B)=>MatrixImpl):MatrixDouble = {
    (lhs.matrix, rhs) match {
      case (Some(lhsm: MatrixImpl), rhsm) => MatrixM(()=>f(lhsm, rhsm))
      case (_, _) => MatrixM.none
    }
  }

  private def ->(matrix:MatrixDouble, f:(MatrixImpl)=>MatrixImpl) : MatrixDouble = {
    matrix.matrix match {
      case Some(matrixm) => MatrixM(()=>f(matrixm))
      case None          => matrix
    }
  }


  implicit val fdouble = new FactoryT {
      override type MatrixImplT = DenseMatrix[Double]
      implicit override def apply(row: Int, col: Int, data: Array[Double]): Option[MatrixImplT] = BreezeDenseMatrix(row, col, data)
      implicit override def apply(row:Int, col:Int): Option[MatrixImplT] = BreezeDenseMatrix(row, col)
  }

  implicit object CompanionT$implicit extends CompanionT {
    override type MatrixImplT = MatrixDouble
    override def fill(row: Int, col: Int, value: ElemT): MatrixDouble = MatrixM(row,col,Array.fill[Double](row*col)(value))
    override def ones(row: Int, col: Int): MatrixDouble   = MatrixM.apply(() => DenseMatrix.ones(row,col))
    override def rand(row: Int, col: Int): MatrixDouble   = MatrixM.apply(() => {DenseMatrix.rand[Double](row, col)})
    override def eye(size: Int): MatrixDouble             = MatrixM.apply(() => DenseMatrix.eye[Double](size))
    override def diag(data: Array[Double]): MatrixDouble  = MatrixM.apply(() => breeze.linalg.diag(new DenseVector[Double](data)))
    override def zeros(row: Int, col: Int): MatrixDouble  = MatrixM.apply(() => DenseMatrix.zeros[Double](row, col))
  }


  implicit class AggregationT$implicit(matrix:MatrixDouble) extends AggregateT[MatrixDouble] {
    override def sumRows(): MatrixDouble = ->(matrix, (m:MatrixImpl)=>breeze.linalg.sum(m(::,*)).toDenseMatrix)

    override def sumCols(): MatrixDouble = ->(matrix, (m:MatrixImpl)=>breeze.linalg.sum(m(*,::)).toDenseMatrix.t)

    override def trace(): Option[ElemT] = {
      matrix.matrix match {
        case Some(m) if m.rows == m.cols => Some(breeze.linalg.trace(m))
        case _ => None
      }
    }
    override def sum(): Option[ElemT] =  ~->(matrix, (m:MatrixImpl)=>breeze.linalg.sum(m))
  }




  implicit class SliceT$implicit(matrix:MatrixDouble) extends SliceT[MatrixDouble] {

    override def apply(row: Int, coll: Int, v: ElemT): MatrixDouble = ->(matrix, (m)=>m(row to row,coll to coll) :=v)

    override def toDiag(): MatrixDouble = ->(matrix, (m:MatrixImpl)=> m :* DenseMatrix.eye[Double](m.rows))

    override def toArray(): Option[Array[ElemT]] = ~->(matrix, (m:MatrixImpl)=>m.toArray)
    def name = "breeze double matrix"  + matrix.toString
    // breeze supports step 1 size only
    override def apply[K, L](row: K, col: L): MatrixDouble = (row,col) match {
      case (r:Range, ::) =>  ->(matrix, (m:MatrixImpl)  => m(r.start to r.end,::))
      case (::, r:Range) =>  ->(matrix, (m:MatrixImpl)  => m(::, r.start to r.end))

      case (row:Int, ::) =>  ->(matrix, (m:MatrixImpl)  => m(row,::).t.toDenseMatrix)
      case (::, col:Int) =>  ->(matrix, (m:MatrixImpl)  => m(::,col).toDenseMatrix.t)

      case (r:Range, c:Range) => ->(matrix, (m:MatrixImpl)  => m(r.start to r.end, c.start to c.end))
      case (row:Int, r:Range) => ->(matrix, (m:MatrixImpl)  => m(row, r.start to r.end).t.toDenseMatrix)
      case (r:Range, col:Int) => ->(matrix, (m:MatrixImpl)  => m(r.start to r.end, col).toDenseMatrix)
      case (_,_) => matrix
    }
    override def apply(row: Int, col: Int): Option[ElemT] = ~->(matrix, (m:MatrixImpl)=>m(row,col))
    override def concatRight(rhs: MatrixDouble): MatrixDouble = ~>(matrix, rhs, (l:MatrixImpl, r:MatrixImpl)=>DenseMatrix.horzcat(l,r))
    override def concatDown(rhs: MatrixDouble): MatrixDouble = ~>(matrix, rhs, (l:MatrixImpl, r:MatrixImpl)=>DenseMatrix.vertcat(l,r))
  }

  implicit class LinearAlgebraT$implicit(matrix: MatrixDouble) extends LinearAlgebraT {
    override type MatrixRetTypeT = MatrixDouble
    override type EigenResultT   = BreezeEigenResult
    override def eig():EigenResultT =  EigenResultM.alloc(matrix.matrix.map(breeze.linalg.eig(_)))
    override def solve(rhs: MatrixDouble): MatrixDouble = ~>(matrix, rhs, (l: MatrixImpl, r: MatrixImpl) => l \ r)
    override def inverse(): MatrixRetTypeT = ->(matrix, (m: MatrixImpl) => inv(m))
    override def transpose(): MatrixDouble = ->(matrix, (m: MatrixImpl) => m.t)
    override def determinant(): Option[ElemT] = ~->(matrix , (m:MatrixImpl)=>det(m))

  }

  implicit object SerializeT$implicit extends SerializeT[MatrixDouble] {
    override def csvWrite(fn: String, matrix:MatrixDouble): Unit = {
      matrix.matrix match {
        case Some(m) => breeze.linalg.csvwrite(new java.io.File(fn),m)
        case _ => Unit
      }
    }
    //

    override def csvRead(fn: String): MatrixDouble = MatrixM(()=>breeze.linalg.csvread(new File(fn)))
  }

  //TODO : Complex eigenvalues/eigenvectors aren't handled
  implicit class EigenResultAccessT$(result : BreezeEigenResult) extends EigenAccessT [MatrixDouble] {
    def name = "jeigen breeze dense matrix result"
    override def values(): MatrixDouble = MatrixM(result.result.map((r)=>DenseMatrix.vertcat(r.eigenvalues.toDenseMatrix, r.eigenvectorsComplex.toDenseMatrix).t))
    //TODO : This will not handle complex eigen vectors.
    override def vectors(): MatrixDouble = MatrixM(result.result.map((r)=>r.eigenvectors))
  }

  implicit object MatrixOperationsTC$implicit$ extends MatrixOperationsTC[MatrixDouble] {
    override def add(lhs: MatrixDouble,   rhs: MatrixDouble): MatrixDouble  = ~>(lhs,rhs, (l:MatrixImpl, r:MatrixImpl)=> l + r)
    override def sub(lhs: MatrixDouble,   rhs: MatrixDouble): MatrixDouble  = ~>(lhs,rhs, (l:MatrixImpl, r:MatrixImpl)=> l - r)
    override def multe(lhs: MatrixDouble, rhs: MatrixDouble):MatrixDouble   = ~>(lhs,rhs, (l:MatrixImpl, r:MatrixImpl)=> l:*r)
    override def dive(lhs: MatrixDouble,  rhs: MatrixDouble): MatrixDouble  = ~>(lhs,rhs, (l:MatrixImpl, r:MatrixImpl)=> l:/r)
    override def mult(lhs: MatrixDouble,  rhs: MatrixDouble): MatrixDouble  = ~>(lhs,rhs, (l:MatrixImpl, r:MatrixImpl)=> l*r)
    override def add1[B: Numeric](lhs: MatrixDouble, rhs: B) = :~>(lhs,rhs, (l:MatrixImpl, r:B)=> BreezeDenseMatrix.adddouble(l.copy, r))
    override def sub1[B: Numeric](lhs: MatrixDouble, rhs: B) = :~>(lhs,rhs, (l:MatrixImpl, r:B)=> BreezeDenseMatrix.subdouble(l.copy, r))
    override def mul1[B: Numeric](lhs: MatrixDouble, rhs: B) = :~>(lhs,rhs, (l:MatrixImpl, r:B)=> BreezeDenseMatrix.muldouble(l.copy, r))
    override def div1[B: Numeric](lhs: MatrixDouble, rhs: B) = :~>(lhs,rhs, (l:MatrixImpl, r:B)=> BreezeDenseMatrix.divdouble(l.copy, r))
    override def eq(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = ~>(lhs, rhs, (l:MatrixImpl, r:MatrixImpl)=> (l :== r).mapValues(boolToDouble))
    override def ne(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = ~>(lhs, rhs, (l:MatrixImpl, r:MatrixImpl)=> (!(l :== r)).mapValues(boolToDouble))
    override def lt(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = ~>(lhs, rhs, (l:MatrixImpl, r:MatrixImpl)=> ((l :< r)).mapValues(boolToDouble))
    override def le(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = ~>(lhs, rhs, (l:MatrixImpl, r:MatrixImpl)=>
      (((l :< r) :| (l :== r)).mapValues(boolToDouble)))
    override def ge(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = ~>(lhs, rhs, (l:MatrixImpl, r:MatrixImpl)=> ((!(l :< r))).mapValues(boolToDouble))
    override def gt(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = ~>(lhs, rhs, (l:MatrixImpl, r:MatrixImpl)=>
      ((!((l :< r) :| (l :== r)))).mapValues(boolToDouble))
    override def create(rows: Int, colls: Int, data: Array[ElemT]): MatrixDouble = MatrixM(rows, colls,data)
    override def zero(row: Int, col: Int): MatrixDouble = MatrixM.zero(row,col)
    override def rand(row: Int, col: Int): MatrixDouble = MatrixM.rand(row,col)
    override def eye(size: Int): MatrixDouble = MatrixM.eye(size)
    override def diag(data: Array[ElemT]): MatrixDouble = MatrixM.diag(data)
    override def ones(row: Int, col: Int): MatrixDouble = MatrixM.ones(row,col)
    override def fill(row: Int, col: Int, value: ElemT): MatrixDouble = MatrixM.fill(row,col,value)
    override def inverse(m: MatrixDouble): MatrixDouble = m.inverse()
    override def solve(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = lhs.solve(rhs)
    override def transpose(m: MatrixDouble): MatrixDouble = m.transpose()
    override def determinant(m: MatrixDouble): Option[ElemT] = m.determinant()
    override def get(m: MatrixDouble, row: Int, coll: Int): Option[ElemT] = m(row,coll)
    override def concatDown(m: MatrixDouble, down: MatrixDouble): MatrixDouble = m concatDown down
    override def set(m: MatrixDouble, row: Int, coll: Int, v: ElemT): MatrixDouble = m(row,coll,v)
    override def toArray(m:MatrixDouble): Option[Array[ElemT]] = m.toArray()
    override def concatRight(m: MatrixDouble, rhs: MatrixDouble): MatrixDouble = m concatRight rhs
    override def toDiag(m: MatrixDouble): MatrixDouble = m.toDiag()
    override def slice[K, L](m: MatrixDouble, row: K, col: L): MatrixDouble = m(row,col)
    override def csvWrite(fn: String, u: MatrixDouble): Unit = MatrixM.csvwrite(fn,u)
    override def csvRead(fn: String): MatrixDouble = MatrixM.csvread(fn)
  }

}

