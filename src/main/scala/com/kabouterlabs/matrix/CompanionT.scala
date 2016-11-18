package com.kabouterlabs.matrix

import scala.annotation.implicitNotFound

/**
  * Created by fons on 3/26/16.
  */

@implicitNotFound("unable to initialize special matrices; CompanionT not implemented")
trait CompanionT {
  type MatrixImplT
  def zeros(row:Int, col:Int)  : MatrixImplT
  def eye(size:Int)            : MatrixImplT
  def rand(row:Int, col:Int)   : MatrixImplT
  def diag(data:Array[Double]) : MatrixImplT
  def ones(row:Int,col:Int)    : MatrixImplT
  def fill(row:Int, col:Int, value:Double): MatrixImplT
}
