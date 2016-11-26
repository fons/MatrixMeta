package com.kabouterlabs.matrix.implicits.armadillojava

/**
  * Created by fons on 11/24/16.
  */

import java.io.{PrintWriter, StringWriter,File}

import com.kabouterlabs.matrix.AggregateT
import com.kabouterlabs.matrix.CompanionT
import com.kabouterlabs.matrix.EigenAccessT
import com.kabouterlabs.matrix.EigenResultM
import com.kabouterlabs.matrix.FactoryT
import com.kabouterlabs.matrix.LinearAlgebraT
import com.kabouterlabs.matrix.MatrixM
import com.kabouterlabs.matrix.MatrixOperations.MatrixOperationsTC
import com.kabouterlabs.matrix.SerializeT
import com.kabouterlabs.matrix.SliceT

import org.armadillojava.{Col, Mat}



import spire.math.Numeric
import scala.io.Source

/**
  * Created by fons on 3/21/16.
  */


private object ArmadilloJavaMat {

  def apply(rows: Int, colls: Int, data: Array[Double]): Option[Mat] = {
    try {
      val f = for (i <- Range(0, rows)) yield Range(i, rows * colls, rows).map(data(_)).toArray
      Some(new Mat(f.toArray))
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

  def apply(rows: Int, colls: Int): Option[Mat] = {
    try {

      Some(org.armadillojava.Arma.zeros(rows,colls))
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

  def csvwrite(file:File, matrix:Mat):Unit = {
    val pw = new PrintWriter(file)
    for (i <- Range(0, matrix.n_rows)) {
      pw.write(matrix.row(i).memptr().mkString(",") ++ "\n")
    }
    pw.close()
  }

  def csvread(file:File):Mat = {
    val l = (for (line <- Source.fromFile(file).getLines()) yield {
      line.split(",").map((s) => s.toDouble)
    }).toArray
    new Mat(l)
  }

  def add[B: Numeric](matrix: Mat, rhs: B): Mat = rhs match {
    case q if rhs.isInstanceOf[Double]  => matrix.plus(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float]  => matrix.plus(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int]  => matrix.plus(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def sub[B: Numeric](matrix: Mat, rhs: B): Mat = rhs match {
    case q if rhs.isInstanceOf[Double]  => matrix.minus(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float]  => matrix.minus(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int]  => matrix.minus(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def mul[B: Numeric](matrix: Mat, rhs: B): Mat = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.times(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.times(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.times(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def div[B: Numeric](matrix: Mat, rhs: B): Mat = rhs match {
    case q if rhs.isInstanceOf[Double]  => matrix.divide(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float]  => matrix.divide(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int]  => matrix.divide(rhs.asInstanceOf[Int])
    case _ => matrix
  }


}

object ArmadilloJavaMatImplicit {
  type ElemT                = Double
  type MatrixImpl           = Mat
  type MatrixDouble         = MatrixM[MatrixImpl]
  type ArmadilloEigenResult = EigenResultM[org.armadillojava.Col]


  private def ->(matrix:MatrixDouble, f:(MatrixImpl)=>MatrixImpl) : MatrixDouble = {
    matrix.matrix match {
      case Some(matrixm) => MatrixM(()=>f(matrixm))
      case None          => matrix
    }
  }

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

  private def @#(matrix:MatrixDouble, a1:Int, a2:Int, a3:Int, a4:Int, f:(MatrixImpl, Int, Int,Int,Int)=>MatrixImpl) : MatrixDouble = {
    matrix.matrix match {

      case Some(matrixm) => MatrixM(()=>f(matrixm,a1,a2,a3,a4))
      case None          => matrix
    }
  }
  //--------------------------------------------------------------------------------------------------------------
  //
  implicit val fdouble$   = new  FactoryT {
    type MatrixImplT = MatrixImpl
    implicit override def apply(row:Int, col:Int, data:Array[Double]):Option[MatrixImplT]  = ArmadilloJavaMat(row,col,data)
    implicit override def apply(row: Int, col: Int): Option[MatrixImplT] = ArmadilloJavaMat(row,col)

  }

  implicit object ArmadilloMatCompanion extends CompanionT {
    override type MatrixImplT = MatrixDouble
    override def fill(row: Int, col: Int, value: ElemT): MatrixDouble = MatrixM(row,col,Array.fill[Double](row*col)(value))
    override def ones(row: Int, col: Int): MatrixDouble = MatrixM.apply(()=>org.armadillojava.Arma.ones(row,col))
    override def rand(row: Int, col: Int): MatrixImplT = MatrixM.apply(()=> org.armadillojava.Arma.randu(row,col))
    override def eye(size: Int): MatrixImplT = MatrixM.apply(()=>org.armadillojava.Arma.eye(size,size))
    override def zeros(row: Int, col: Int): MatrixImplT =  MatrixM.apply(()=>org.armadillojava.Arma.zeros(row,col))
    //TODO check this
    override def diag(data: Array[Double]): MatrixImplT = {
      fdouble$(data.length, 1, data) match {
        case None     =>  new MatrixDouble(None)
        case Some(a)  =>  MatrixM.apply(()=>org.armadillojava.Arma.diagmat(new Col(a)))
      }
    }
  }


  implicit class AggregationT$implicit(matrix:MatrixDouble) extends AggregateT[MatrixDouble] {
    override def sumRows(): MatrixDouble = ->(matrix, (m:Mat) => org.armadillojava.Arma.cumsum(m,0).rows(m.n_rows - 1, m.n_rows-1))
    override def sumCols(): MatrixDouble = ->(matrix, (m:Mat)=> org.armadillojava.Arma.cumsum(m,1).cols(m.n_cols-1, m.n_cols-1))
    override def trace(): Option[ElemT] = {
      matrix.matrix match {
        case Some(m)  => Some(org.armadillojava.Arma.trace(m)) //TODO may throw exception
        case _ => None
      }
    }
    override def sum(): Option[ElemT] =  ~->(matrix, (m:Mat)=>org.armadillojava.Arma.accu(m))
  }

  implicit class SliceT$implicit(matrix:MatrixDouble) extends SliceT[MatrixDouble] {

    override def apply(row: Int, coll: Int, v: ElemT): MatrixDouble =  ->(matrix, (m:Mat)=>{m.at(row,coll,org.armadillojava.Op.EQUAL,v); m})

    override def toDiag(): MatrixDouble = ->(matrix, (m:Mat)=> org.armadillojava.Arma.diagmat(m))

    override def concatRight(rhs: MatrixDouble): MatrixDouble   = ~>(matrix, rhs, (l:Mat, r:Mat)=> org.armadillojava.Arma.join_horiz(l,r))

    override def concatDown(rhs: MatrixDouble): MatrixDouble    = ~>(matrix, rhs, (l:Mat, r:Mat)=> org.armadillojava.Arma.join_vert(l,r))

    override def toArray(): Option[Array[ElemT]] = ~->(matrix, (m:Mat)=>m.memptr())
    override def apply(row: Int, coll: Int): Option[Double] = ~->(matrix, (m:Mat)=>m.at(row,coll))
    override def apply[K, L](row: K, col: L): MatrixDouble = (row,col) match {
      case (r:Range, `::`)       => @#(matrix, r.start, r.end,0,0, (m:Mat, start:Int,end:Int, _,_)=> m.rows(start, end))
      case (`::`, r:Range)       => @#(matrix, r.start, r.end,0,0, (m:Mat, start:Int,end:Int, _,_)=> m.cols(start, end))
      case (row:Int, `::`)       => @#(matrix, row, 0,0,0, (m:Mat, row:Int,_, _,_)=> m.rows(row,row))
      case (`::`, col:Int)       => @#(matrix, col, 0,0,0, (m:Mat, col:Int, _, _, _)=> m.cols(col,col))
      case (r:Range, c:Range)    => @#(matrix, r.start, r.end+1,c.start,c.end+1, (m:Mat, rs,re, cs,ce)=> m.submat(rs,re,cs,ce))
      case (row : Int, r:Range ) => @#(matrix, row, row+1,r.start,r.end+1, (m:Mat, rs,re, cs,ce)=> m.submat(rs,re,cs,ce))
      case (r:Range, col:Int )   => @#(matrix, r.start, r.end+1,col,col+1, (m:Mat, rs,re, cs,ce)=> m.submat(rs,re,cs,ce))
      case(_,_) => matrix
    }
  }



  implicit class LinearAlgebraT$implicit(matrix:MatrixDouble) extends LinearAlgebraT {

    override type MatrixRetTypeT = MatrixDouble
    override type EigenResultT   = ArmadilloEigenResult
    override def inverse():MatrixRetTypeT = ->(matrix, (m:Mat)=> m.i())
    //TODO : can be optimized based on matrix type..
    override def solve(rhs: MatrixDouble): MatrixRetTypeT = ~>(matrix, rhs, (l:MatrixImpl, r:MatrixImpl)=>  org.armadillojava.Arma.solve(l,r))
    override def eig(): EigenResultT = EigenResultM.alloc(None)
    override def transpose(): MatrixRetTypeT = ->(matrix, (m:Mat)=> m.t)
    override def determinant(): Option[Double] = ~->(matrix, (m:Mat)=> org.armadillojava.Arma.det(m))
  }

  implicit object SerializeT$implicit extends SerializeT[MatrixDouble] {
    override def csvWrite(fn: String, matrix: MatrixDouble): Unit  =  {
      matrix.matrix match {
        case Some(m) => ArmadilloJavaMat.csvwrite(new File(fn), m)
        case None => {}
      }
    }


    override def csvRead(fn: String): MatrixDouble = MatrixM(()=>ArmadilloJavaMat.csvread(new File(fn)))
  }
  //TODO : Complex eigenvalues/eigenvectors aren't handled

//  implicit class EigenAccessT$implicit(result : ArmadilloEigenResult) extends EigenAccessT [MatrixDouble] {
//    def name = "Armadillo result"
//    override def vectors(): MatrixDouble = MatrixM.zero(1,1)//MatrixM(result.result.map((r)=>r.vectors.real().concatRight(r.vectors.imag())))
//    override def values(): MatrixDouble  = MatrixM(result.result.get.memptr().size,1, result.result.get.memptr())
//
//  }

  implicit object MatrixOperationsTC$implicit$ extends MatrixOperationsTC[MatrixDouble] {
    override def add(lhs: MatrixDouble, rhs: MatrixDouble):MatrixDouble    = ~>(lhs, rhs, (lhsm:Mat, rhsm:Mat)=>lhsm.plus(rhsm))
    override def sub(lhs: MatrixDouble, rhs: MatrixDouble):MatrixDouble    = ~>(lhs, rhs, (lhsm:Mat, rhsm:Mat)=>lhsm.minus(rhsm))
    override def mult(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble  = ~>(lhs, rhs, (lhsm:Mat, rhsm:Mat)=>lhsm.times(rhsm))
    override def multe(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = ~>(lhs, rhs, (lhsm:Mat, rhsm:Mat)=>{lhsm.elemTimes(rhsm)})
    override def dive(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble  = ~>(lhs, rhs, (lhsm:Mat, rhsm:Mat)=>{lhsm.elemDivide(rhsm)})

    override def add1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble =
      :~>[B](lhs:MatrixDouble, rhs:B, (lhs:Mat, rhs:B)=>ArmadilloJavaMat.add(lhs, rhs))
    override def sub1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble =
      :~>[B](lhs:MatrixDouble, rhs:B, (lhs:Mat, rhs:B)=>ArmadilloJavaMat.sub(lhs, rhs))
    override def mul1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble =
      :~>[B](lhs:MatrixDouble, rhs:B, (lhs:Mat, rhs:B)=>ArmadilloJavaMat.mul(lhs, rhs))
    override def div1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble =
      :~>[B](lhs:MatrixDouble, rhs:B, (lhs:Mat, rhs:B)=>ArmadilloJavaMat.div(lhs, rhs))
    override def eq(lhs:MatrixDouble, rhs:MatrixDouble):MatrixDouble = ~>(lhs, rhs, (lhsm:Mat, rhsm:Mat)=>lhsm.equals(rhsm))
    override def ne(lhs:MatrixDouble, rhs:MatrixDouble):MatrixDouble = ~>(lhs, rhs, (lhsm:Mat, rhsm:Mat)=>lhsm.nonEquals(rhsm))
    override def gt(lhs:MatrixDouble, rhs:MatrixDouble):MatrixDouble = ~>(lhs, rhs, (lhsm:Mat, rhsm:Mat)=>lhsm.strictGreaterThan(rhsm))
    override def ge(lhs:MatrixDouble, rhs:MatrixDouble):MatrixDouble = ~>(lhs, rhs, (lhsm:Mat, rhsm:Mat)=>lhsm.greaterThan(rhsm))
    override def lt(lhs:MatrixDouble, rhs:MatrixDouble):MatrixDouble = ~>(lhs, rhs, (lhsm:Mat, rhsm:Mat)=>lhsm.strictLessThan(rhsm))
    override def le(lhs:MatrixDouble, rhs:MatrixDouble):MatrixDouble = ~>(lhs, rhs, (lhsm:Mat, rhsm:Mat)=>lhsm.lessThan(rhsm))
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

