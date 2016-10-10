package com.kabouterlabs.matrix

/**
  * Created by fons on 3/14/16.
  */


trait FactoryT {
  type MatrixImplT
  def apply(row:Int, col:Int, data:Array[Double]):Option[MatrixImplT]
  def apply(row:Int, col:Int):Option[MatrixImplT]
}





