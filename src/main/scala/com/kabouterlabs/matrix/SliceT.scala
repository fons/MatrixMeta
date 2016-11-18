package com.kabouterlabs.matrix

/**
  * Created by fons on 3/27/16.
  */

trait SliceT[MatrixImplT] {
  def apply(row:Int, coll:Int):Option[Double]
  def apply(row:Int, coll:Int, v:Double):MatrixImplT
  def apply[K,L](row:K, col:L):MatrixImplT
  def toArray() : Option[Array[Double]]
  def concatRight(matrix: MatrixImplT):MatrixImplT
  def concatDown(matrix: MatrixImplT):MatrixImplT
  def toDiag() : MatrixImplT

}

