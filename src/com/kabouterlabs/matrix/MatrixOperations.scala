package com.kabouterlabs.matrix

/**
  * Created by fons on 3/20/16.
  */

import spire.math.Numeric


object MatrixOperations {


  trait MatrixOperationsTC[A] {

    def add(x: A, y: A): A
    def sub(x: A, y: A): A
    def mult(lhs:A, rhs:A):A
    def multe(lhs:A, rhs:A):A
    def dive(lhs:A, rhs:A):A
    def add1[B:Numeric](x:A, y:B):A
    def sub1[B:Numeric](x:A, y:B):A
    def mul1[B:Numeric](x:A, y:B):A
    def div1[B:Numeric](x:A, y:B):A
    def eq(lhs:A, rhs:A):A
    def ne(lhs:A, rhs:A):A
    def ge(lhs:A, rhs:A):A
    def le(lhs:A, rhs:A):A
    def gt(lhs:A, rhs:A):A
    def lt(lhs:A, rhs:A):A
    def create(rows:Int, colls:Int, data:Array[Double]):A
    def zero(row:Int, col:Int)  :A
    def eye(size:Int)            :A
    def rand(row:Int, col:Int)   :A
    def diag(data:Array[Double]) :A
    def ones(row:Int,col:Int)    :A
    def fill(row:Int, col:Int, value:Double):A
    def inverse(m:A):A
    def transpose(m:A):A
    def determinant(m:A):Option[Double]
    def solve(lhs:A, rhs:A):A
    def get(m:A, row:Int, coll:Int):Option[Double]
    def set(m:A, row:Int, coll:Int, v:Double):A
    def slice[K,L](m:A, row:K, col:L):A
    def toArray(m:A) : Option[Array[Double]]
    def concatRight(m:A, rhs: A):A
    def concatDown(m:A, down: A):A
    def toDiag(m:A) : A
    def csvWrite(fn:String, u:A):Unit
    def csvRead(fn:String):A
  }


  //////////////////////////////////////////////////////////////////////

  implicit class MatrixOperator$[A](lhs: A)(implicit ev: MatrixOperationsTC[A]) {
    def :+(rhs: A) = ev.add(lhs, rhs)
    def :-(rhs: A) = ev.sub(lhs, rhs)
    def |*(rhs:A)  = ev.mult(lhs, rhs)
    def :*(rhs:A)  = ev.multe(lhs, rhs)
    def :\(rhs:A)  = ev.dive(lhs, rhs)
    def ++[B:Numeric](rhs:B) = ev.add1(lhs,rhs)
    def --[B:Numeric](rhs:B) = ev.sub1(lhs,rhs)
    def **[B:Numeric](rhs:B) = ev.mul1(lhs,rhs)
    def \\[B:Numeric](rhs:B) = ev.div1(lhs,rhs)
    def :==(rhs:A):A = ev.eq(lhs, rhs)
    def :!=(rhs:A):A = ev.ne(lhs, rhs)
    def :>=(rhs:A):A = ev.ge(lhs, rhs)
    def :<=(rhs:A):A = ev.le(lhs, rhs)
    def :>>(rhs:A):A = ev.gt(lhs, rhs)
    def :<<(rhs:A):A = ev.lt(lhs, rhs)
  }

  def add[A:MatrixOperationsTC](lhs:A, rhs:A)   = lhs  :+ rhs
  def sub[A:MatrixOperationsTC](lhs:A, rhs:A)   = lhs  :- rhs
  def mulm[A:MatrixOperationsTC](lhs:A, rhs:A)  = lhs |* rhs
  def mul[A:MatrixOperationsTC](lhs:A, rhs:A)   = lhs :* rhs
  def div[A:MatrixOperationsTC](lhs:A, rhs:A)   = lhs  :\ rhs

  def add1[A:MatrixOperationsTC, B:Numeric](lhs:A, rhs:B)   = lhs ++ rhs
  def sub1[A:MatrixOperationsTC, B:Numeric](lhs:A, rhs:B)   = lhs -- rhs
  def mul1[A:MatrixOperationsTC, B:Numeric](lhs:A, rhs:B)   = lhs ** rhs
  def div1[A:MatrixOperationsTC, B:Numeric](lhs:A, rhs:B)   = lhs \\ rhs

  def meq[A:MatrixOperationsTC](lhs:A, rhs:A) = lhs :== rhs
  def mne[A:MatrixOperationsTC](lhs:A, rhs:A) = lhs :!= rhs
  def mge[A:MatrixOperationsTC](lhs:A, rhs:A) = lhs :>= rhs
  def mle[A:MatrixOperationsTC](lhs:A, rhs:A) = lhs :<= rhs
  def mgt[A:MatrixOperationsTC](lhs:A, rhs:A) = lhs :>> rhs
  def mlt[A:MatrixOperationsTC](lhs:A, rhs:A) = lhs :<< rhs




}
