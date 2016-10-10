package com.kabouterlabs.matrix

/**
  * Created by fons on 4/18/16.
  */
trait AggregateT[MatrixImplT] {
  def trace()   : Option[Double]
  def sum()     : Option[Double]
  def sumRows   : MatrixImplT
  def sumCols() : MatrixImplT
}


