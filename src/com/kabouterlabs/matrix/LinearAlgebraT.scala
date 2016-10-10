package com.kabouterlabs.matrix

/**
  * Created by fons on 3/28/16.
  */
trait LinearAlgebraT {
  type MatrixRetTypeT
  type EigenResultT
  def inverse():MatrixRetTypeT
  def transpose():MatrixRetTypeT
  def determinant():Option[Double]
  def solve(rhs:MatrixRetTypeT):MatrixRetTypeT
  def eig():EigenResultT
}

