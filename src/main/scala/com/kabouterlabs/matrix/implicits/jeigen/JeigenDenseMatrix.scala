package com.kabouterlabs.matrix.implicits.jeigen

/**
  * Created by fons on 3/24/16.
  */

import java.io.{PrintWriter, StringWriter,File}

import com.kabouterlabs.matrix.MatrixOperations.MatrixOperationsTC
import com.kabouterlabs.matrix._
import jeigen.DenseMatrix
import spire.math.Numeric
import scala.io.Source

/**
  * Created by fons on 3/21/16.
  */


private object JeigenDenseMatrix {

  def apply(rows: Int, colls: Int, data: Array[Double]): Option[DenseMatrix] = {
    try {
      val f = for (i <- Range(0, rows)) yield Range(i, rows * colls, rows).map(data(_)).toArray
      Some(new DenseMatrix(f.toArray))
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

  def apply(rows: Int, colls: Int): Option[DenseMatrix] = {
    try {

      Some(jeigen.Shortcuts.zeros(rows,colls))
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



  def csvwrite(file:File, matrix:DenseMatrix):Unit = {
    val pw = new PrintWriter(file)
    for (i <- Range(0, matrix.rows)) {
      pw.write(matrix.row(i).getValues().mkString(",") ++ "\n")
    }
    pw.close()
  }

  def csvread(file:File):DenseMatrix = {
      val l = (for (line <- Source.fromFile(file).getLines()) yield {
        line.split(",").map((s) => s.toDouble)
      }).toArray
      new DenseMatrix(l)
  }

  def add[B: Numeric](matrix: DenseMatrix, rhs: B): DenseMatrix = rhs match {
    case q if rhs.isInstanceOf[Double]  => matrix.add(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float]  => matrix.add(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int]  => matrix.add(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def sub[B: Numeric](matrix: DenseMatrix, rhs: B): DenseMatrix = rhs match {
    case q if rhs.isInstanceOf[Double]  => matrix.sub(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float]  => matrix.sub(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int]  => matrix.sub(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def mul[B: Numeric](matrix: DenseMatrix, rhs: B): DenseMatrix = rhs match {
    case q if rhs.isInstanceOf[Double] => matrix.mul(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float] => matrix.mul(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int] => matrix.mul(rhs.asInstanceOf[Int])
    case _ => matrix
  }

  def div[B: Numeric](matrix: DenseMatrix, rhs: B): DenseMatrix = rhs match {
    case q if rhs.isInstanceOf[Double]  => matrix.div(rhs.asInstanceOf[Double])
    case q if rhs.isInstanceOf[Float]  => matrix.div(rhs.asInstanceOf[Float])
    case q if rhs.isInstanceOf[Int]  => matrix.div(rhs.asInstanceOf[Int])
    case _ => matrix
  }


}

object JeigenDenseMatrixImplicit {
  type ElemT             = Double
  type MatrixImpl        = DenseMatrix
  type MatrixDouble      = MatrixM[MatrixImpl]
  type JeigenEigenResult = EigenResultM[jeigen.DenseMatrix.EigenResult]


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
    implicit override def apply(row:Int, col:Int, data:Array[Double]):Option[MatrixImplT]  = JeigenDenseMatrix(row,col,data)
    implicit override def apply(row: Int, col: Int): Option[MatrixImplT] = JeigenDenseMatrix(row,col)

  }

  implicit object JeigenDenseMatrixCompanion extends CompanionT {
    override type MatrixImplT = MatrixDouble
    override def fill(row: Int, col: Int, value: ElemT): MatrixDouble = MatrixM(row,col,Array.fill[Double](row*col)(value))
    override def ones(row: Int, col: Int): MatrixDouble = MatrixM.apply(()=>jeigen.Shortcuts.ones(row,col))
    override def rand(row: Int, col: Int): MatrixImplT = MatrixM.apply(()=> jeigen.Shortcuts.rand(row,col))
    override def eye(size: Int): MatrixImplT = MatrixM.apply(()=>jeigen.Shortcuts.eye(size))
    override def zeros(row: Int, col: Int): MatrixImplT =  MatrixM.apply(()=>jeigen.Shortcuts.zeros(row,col))
    //TODO check this
    override def diag(data: Array[Double]): MatrixImplT = {
      fdouble$(data.length, 1, data) match {
        case None     =>  new MatrixDouble(None)
        case Some(a)  =>  MatrixM.apply(()=>jeigen.Shortcuts.diag(a))
      }
    }
  }


  implicit class AggregationT$implicit(matrix:MatrixDouble) extends AggregateT[MatrixDouble] {
    override def sumRows(): MatrixDouble = ->(matrix, (m:DenseMatrix)=>m.sumOverRows() )
    override def sumCols(): MatrixDouble = ->(matrix, (m:DenseMatrix)=>m.sumOverCols() )
    override def trace(): Option[ElemT] = {
      matrix.matrix match {
        case Some(m) if m.rows == m.cols => Some((for(i <- Range(0,m.rows)) yield { m.get(i,i) }).sum)
        case _ => None
      }
    }
    override def sum(): Option[ElemT] =  ~->(matrix, (m:DenseMatrix)=>m.sum().sum().s())
  }

  implicit class SliceT$implicit(matrix:MatrixDouble) extends SliceT[MatrixDouble] {

    override def apply(row: Int, coll: Int, v: ElemT): MatrixDouble =  ->(matrix, (m:DenseMatrix)=>{m.set(row,coll,v); m})

    override def toDiag(): MatrixDouble = ->(matrix, (m:DenseMatrix)=> m.mul(jeigen.Shortcuts.eye(m.rows)))

    override def concatRight(rhs: MatrixDouble): MatrixDouble   = ~>(matrix, rhs, (l:DenseMatrix, r:DenseMatrix)=>l.concatRight(r))
    override def concatDown(rhs: MatrixDouble): MatrixDouble    = ~>(matrix, rhs, (l:DenseMatrix, r:DenseMatrix)=>l.concatDown(r))

    override def toArray(): Option[Array[ElemT]] = ~->(matrix, (m:DenseMatrix)=>m.getValues)
    override def apply(row: Int, coll: Int): Option[Double] = ~->(matrix, (m:DenseMatrix)=>m.get(row,coll))
    override def apply[K, L](row: K, col: L): MatrixDouble = (row,col) match {
      case (r:Range, `::`)       => @#(matrix, r.start, r.end,0,0, (m:DenseMatrix, start:Int,end:Int, _,_)=> m.rows(start, end + 1))
      case (`::`, r:Range)       => @#(matrix, r.start, r.end,0,0, (m:DenseMatrix, start:Int,end:Int, _,_)=> m.cols(start, end + 1))
      case (row:Int, `::`)       => @#(matrix, row, 0,0,0, (m:DenseMatrix, row:Int,_, _,_)=> m.row(row))
      case (`::`, col:Int)       => @#(matrix, col, 0,0,0, (m:DenseMatrix, col:Int, _, _, _)=> m.col(col))
      case (r:Range, c:Range)    => @#(matrix, r.start, r.end+1,c.start,c.end+1, (m:DenseMatrix, rs,re, cs,ce)=> m.slice(rs,re,cs,ce))
      case (row : Int, r:Range ) => @#(matrix, row, row+1,r.start,r.end+1, (m:DenseMatrix, rs,re, cs,ce)=> m.slice(rs,re,cs,ce))
      case (r:Range, col:Int )   => @#(matrix, r.start, r.end+1,col,col+1, (m:DenseMatrix, rs,re, cs,ce)=> m.slice(rs,re,cs,ce))
      case(_,_) => matrix
    }
  }

  implicit class LinearAlgebraT$implicit(matrix:MatrixDouble) extends LinearAlgebraT {

    override type MatrixRetTypeT = MatrixDouble
    override type EigenResultT   = JeigenEigenResult
    override def inverse():MatrixRetTypeT = ->(matrix, (m:DenseMatrix)=> m.fullPivHouseholderQRSolve(jeigen.Shortcuts.eye(m.cols)))
    //TODO : can be optimized based on matrix type..
    override def solve(rhs: MatrixDouble): MatrixRetTypeT = ~>(matrix, rhs, (l:MatrixImpl, r:MatrixImpl)=>l.fullPivHouseholderQRSolve(r))
    override def eig(): EigenResultT = EigenResultM.alloc(matrix.matrix.map(_.eig()))
    override def transpose(): MatrixRetTypeT = ->(matrix, (m:DenseMatrix)=> m.t)
    override def determinant(): Option[Double] = ~->(matrix, (m)=>m.eig().values.real().getValues().foldLeft[Double](1.0)(_ * _))
  }

  implicit object SerializeT$implicit extends SerializeT[MatrixDouble] {
    override def csvWrite(fn: String, matrix: MatrixDouble): Unit  =  {
      matrix.matrix match {
        case Some(m) => JeigenDenseMatrix.csvwrite(new File(fn), m)
        case None => {}
      }
    }


    override def csvRead(fn: String): MatrixDouble = MatrixM(()=>JeigenDenseMatrix.csvread(new File(fn)))
  }
  //TODO : Complex eigenvalues/eigenvectors aren't handled

  implicit class EigenAccessT$implicit(result : JeigenEigenResult) extends EigenAccessT [MatrixDouble] {
    def name = "jeigen result"
    override def vectors(): MatrixDouble = MatrixM(result.result.map((r)=>r.vectors.real().concatRight(r.vectors.imag())))
    override def values(): MatrixDouble  = MatrixM(result.result.map((r) => r.values.real().concatRight(r.values.imag())))

  }

  implicit object MatrixOperationsTC$implicit$ extends MatrixOperationsTC[MatrixDouble] {
    override def add(lhs: MatrixDouble, rhs: MatrixDouble):MatrixDouble    = ~>(lhs, rhs, (lhsm:DenseMatrix, rhsm:DenseMatrix)=>lhsm.add(rhsm))
    override def sub(lhs: MatrixDouble, rhs: MatrixDouble):MatrixDouble    = ~>(lhs, rhs, (lhsm:DenseMatrix, rhsm:DenseMatrix)=>lhsm.sub(rhsm))
    override def mult(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble  = ~>(lhs, rhs, (lhsm:DenseMatrix, rhsm:DenseMatrix)=>lhsm.mmul(rhsm))
    override def multe(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble = ~>(lhs, rhs, (lhsm:DenseMatrix, rhsm:DenseMatrix)=>lhsm.mul(rhsm))
    override def dive(lhs: MatrixDouble, rhs: MatrixDouble): MatrixDouble  = ~>(lhs, rhs, (lhsm:DenseMatrix, rhsm:DenseMatrix)=>lhsm.div(rhsm))
    override def add1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble =
      :~>[B](lhs:MatrixDouble, rhs:B, (lhs:DenseMatrix, rhs:B)=>JeigenDenseMatrix.add(lhs, rhs))
    override def sub1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble =
      :~>[B](lhs:MatrixDouble, rhs:B, (lhs:DenseMatrix, rhs:B)=>JeigenDenseMatrix.sub(lhs, rhs))
    override def mul1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble =
      :~>[B](lhs:MatrixDouble, rhs:B, (lhs:DenseMatrix, rhs:B)=>JeigenDenseMatrix.mul(lhs, rhs))
    override def div1[B: Numeric](lhs: MatrixDouble, rhs: B): MatrixDouble =
      :~>[B](lhs:MatrixDouble, rhs:B, (lhs:DenseMatrix, rhs:B)=>JeigenDenseMatrix.div(lhs, rhs))
    override def eq(lhs:MatrixDouble, rhs:MatrixDouble):MatrixDouble = ~>(lhs, rhs, (lhsm:DenseMatrix, rhsm:DenseMatrix)=>lhsm.eq(rhsm))
    override def ne(lhs:MatrixDouble, rhs:MatrixDouble):MatrixDouble = ~>(lhs, rhs, (lhsm:DenseMatrix, rhsm:DenseMatrix)=>lhsm.ne(rhsm))
    override def gt(lhs:MatrixDouble, rhs:MatrixDouble):MatrixDouble = ~>(lhs, rhs, (lhsm:DenseMatrix, rhsm:DenseMatrix)=>lhsm.gt(rhsm))
    override def ge(lhs:MatrixDouble, rhs:MatrixDouble):MatrixDouble = ~>(lhs, rhs, (lhsm:DenseMatrix, rhsm:DenseMatrix)=>lhsm.ge(rhsm))
    override def lt(lhs:MatrixDouble, rhs:MatrixDouble):MatrixDouble = ~>(lhs, rhs, (lhsm:DenseMatrix, rhsm:DenseMatrix)=>lhsm.lt(rhsm))
    override def le(lhs:MatrixDouble, rhs:MatrixDouble):MatrixDouble = ~>(lhs, rhs, (lhsm:DenseMatrix, rhsm:DenseMatrix)=>lhsm.le(rhsm))
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