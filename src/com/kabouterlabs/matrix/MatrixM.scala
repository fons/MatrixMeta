package com.kabouterlabs.matrix

import java.io.{PrintWriter, StringWriter}

/**
  * Created by fons on 3/19/16.
  */



case class MatrixM[V](matrix: Option[V]) {
   override def toString = "{" + matrix.getOrElse(None).getClass.getName + "\n" + matrix.getOrElse(None).toString + "}"
}

object MatrixM
{

  def apply(row:Int, col:Int, data:Array[Double])(implicit factory: FactoryT) = new MatrixM[factory.MatrixImplT](factory(row,col,data))
  def apply(row:Int, col:Int)(implicit factory: FactoryT) = new MatrixM[factory.MatrixImplT](factory(row,col))

  def apply(row:Int, col:Int, data:Option[Array[Double]])(implicit factory: FactoryT) = {
    data match {
      case Some(_data_)  => new MatrixM[factory.MatrixImplT](factory(row, col, _data_))
      case None =>   new MatrixM[factory.MatrixImplT](None)
    }
  }



  def zero(row:Int, colls:Int)(implicit companion: CompanionT):companion.MatrixImplT = companion.zeros(row, colls)
  def eye(size:Int)(implicit companion: CompanionT):companion.MatrixImplT = companion.eye(size)
  def rand(row:Int, colls:Int)(implicit companion: CompanionT):companion.MatrixImplT = companion.rand(row,colls)
  def diag(data:Array[Double]) (implicit companion: CompanionT):companion.MatrixImplT = companion.diag(data)
  def ones(row:Int, col:Int) (implicit  companion: CompanionT):companion.MatrixImplT = companion.ones(row,col)
  def fill(row:Int, col:Int, value:Double) (implicit  companion: CompanionT):companion.MatrixImplT = companion.fill(row,col,value)
  def csvwrite[U](fn:String, u:U)(implicit serializeT: SerializeT[U]):Unit = serializeT.csvWrite(fn, u)
  def csvread[U](fn:String)(implicit serializeT: SerializeT[U]) = serializeT.csvRead(fn)
  def <=[U](fn:String)(implicit serializeT: SerializeT[U]) = serializeT.csvRead(fn)
  def none[U] = new MatrixM[U](None)

  def apply[U](u:U):MatrixM[U] ={
    try {
      new MatrixM[U](Some(u))
    }
    catch {
      case e:Throwable => {
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        new MatrixM[U](None)
      }
    }
  }

  def apply[U](f:() => U) : MatrixM[U]  = {
    try {
      new MatrixM[U](Some(f()))
    }
    catch {
      case e:Throwable => {
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        new MatrixM[U](None)

      }
    }

  }
}

