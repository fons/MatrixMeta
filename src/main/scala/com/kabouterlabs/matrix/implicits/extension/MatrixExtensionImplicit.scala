/*
 * Copyright (c) 2016.
 * https://opensource.org/licenses/BSD-3-Clause
 *
 * Copyright (c) 2016, MatrixMeta developers
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list
 *    of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO,THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 * THE COPYRIGHT HOLDER OR CONTRIBUTORS BELIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * EOM
 */

package com.kabouterlabs.matrix.implicits.extension

import com.kabouterlabs.matrix.MatrixExtension.MatrixExtensionsTC
import com.kabouterlabs.matrix.{StatT, MathT, MapT, MatrixM}
import com.kabouterlabs.matrix.MatrixOperations.MatrixOperationsTC

import scala.annotation.tailrec

/**
  * Created by fons on 12/30/16.
  */
object MatrixExtensionImplicit {
  object MatrixMapper {

    private def mapDouble[U](matrix: MatrixM[U], f:(Double)=>Double)(implicit ev: MatrixOperationsTC[MatrixM[U]]{type MatrixDataElemT = Double}):MatrixM[U] ={
      for (row <- Range(0, ev.rows(matrix)); col <- Range(0, ev.columns(matrix))) {
      ev.get(matrix, row, col).map(f) match {
        case Some(value) => ev.set(matrix, row, col, value)
        case None => ev.none
      }
    }
    matrix
  }
    private type reduceT = (Double, Double)=>Double
    private def reduceDouble[U](matrix: MatrixM[U], v:Double, f: reduceT)(implicit ev: MatrixOperationsTC[MatrixM[U]]{type MatrixDataElemT = Double}):Option[Double] = ev.toArray(matrix).map((arr)=> arr.fold(v)(f))

    implicit class Ev$MapT[U](matrix: MatrixM[U])(implicit ev: MatrixOperationsTC[MatrixM[U]]{type MatrixDataElemT = Double}) extends MapT [MatrixM[U]] {
      type M =  MatrixOperationsTC[MatrixM[U]]{type MatrixDataElemT = Double}
      type ContElemT = M#MatrixDataElemT

      def mapFilter(f: (Int, Int, ContElemT) => ContElemT): MatrixM[U] =
       {

        for (row <- Range(0, ev.rows(matrix)); col <- Range(0, ev.columns(matrix))) yield {
          ev.set(matrix, row, col, ev.get(matrix, row, col).map((x) => f(row, col, x)).get)
        }
        matrix
      }

      def reduceFunc(v: ContElemT)(f: (ContElemT, ContElemT) => ContElemT):Option[ContElemT] = reduceDouble(matrix,v, f)

      override def mapFunc(f: (ContElemT) => ContElemT) = mapDouble(matrix, f)

      override def foldFunc[W](w: W)(f: (W, ContElemT) => W): Option[W] = ev.toArray(matrix).map((arr)=> arr.foldLeft[W](w)(f))

      override def reduceFilter(v: M#MatrixDataElemT)(f: (M#MatrixDataElemT, Int, Int, M#MatrixDataElemT) => M#MatrixDataElemT): Option[M#MatrixDataElemT] = {
        val m_rows = ev.rows(matrix)
        val m_cols = ev.columns(matrix)
        val m_row_last = m_rows - 1
        val m_col_last = m_cols - 1
        @tailrec
        def oprecurse(m:MatrixM[U], row:Int, col:Int, accumOpt:Option[ContElemT]) :Option[ContElemT] = {
          (row, col, ev.get(m,row,col), accumOpt) match {
            case (r,c, Some(mval), Some(accum)) if r < m_rows      && c <  m_col_last => oprecurse(m,r,   c+1, Some(f(accum,r,c,mval)))
            case (r,c, Some(mval), Some(accum)) if r < m_row_last  && c == m_col_last => oprecurse(m,r+1, 0,   Some(f(accum,r,c,mval)))
            case (r,c, Some(mval), Some(accum)) if r == m_row_last && c == m_col_last => Some(f(accum,r,c,mval))
            case (_,_,_,_) => None
          }
        }
        oprecurse(matrix,0,0,Some(v))
      }
    }//end of implicit


    implicit class Ev$MathT[U](matrix: MatrixM[U])(implicit ev: MatrixOperationsTC[MatrixM[U]]{type MatrixDataElemT = Double}) extends MathT [MatrixM[U]] {

      type M =  MatrixOperationsTC[MatrixM[U]]{type MatrixDataElemT = Double}
      override type ContElemT = M#MatrixDataElemT

      override def atan: MatrixM[U] = mapDouble(matrix, scala.math.atan)

      override def acos: MatrixM[U] = mapDouble(matrix,  scala.math.acos)

      override def log10: MatrixM[U] = mapDouble(matrix,  scala.math.log10)

      override def tanh: MatrixM[U] = mapDouble(matrix,  scala.math.tanh)

      override def log: MatrixM[U] = mapDouble(matrix,  scala.math.log)

      override def round: MatrixM[U] = mapDouble(matrix,  (x) => scala.math.round(x)* 1.0)

      override def cosh: MatrixM[U] = mapDouble(matrix,  scala.math.cosh)

      override def tan: MatrixM[U] = mapDouble(matrix,  scala.math.tan)

      override def cos: MatrixM[U] = mapDouble(matrix,  scala.math.cos)

      override def exp: MatrixM[U] = mapDouble(matrix,  scala.math.exp)

      override def expm1: MatrixM[U] = mapDouble(matrix,  scala.math.expm1)

      override def pow(p: ContElemT): MatrixM[U] = mapDouble(matrix,  scala.math.pow(_,p))

      override def asinh: MatrixM[U] = mapDouble(matrix, (x) => scala.math.log(x + scala.math.sqrt(1.0 + x*x)))

      override def asin: MatrixM[U] = mapDouble(matrix,  scala.math.asin)

      override def floor: MatrixM[U] = mapDouble(matrix,  scala.math.floor)

      override def abs: MatrixM[U] = mapDouble(matrix,  scala.math.abs)

      override def sqrt: MatrixM[U] = mapDouble(matrix,  scala.math.sqrt)

      override def log1p: MatrixM[U] = mapDouble(matrix,  scala.math.log1p)

      override def sin: MatrixM[U] = mapDouble(matrix,  scala.math.sin)

      override def atanh: MatrixM[U] = mapDouble(matrix, (x) => scala.math.log((1.0 +x) /(1.0 - x)))

      override def ceil: MatrixM[U] = mapDouble(matrix,  scala.math.ceil)

      override def acosh: MatrixM[U] = mapDouble(matrix, (x) => scala.math.log(x + scala.math.sqrt(x*x - 1)))

      override def sinh: MatrixM[U] = mapDouble(matrix,  scala.math.sinh)
    }

    implicit class Ev$StatT[U](matrix: MatrixM[U])(implicit ev: MatrixOperationsTC[MatrixM[U]]{type MatrixDataElemT = Double}) extends StatT [MatrixM[U]] {

      type M = MatrixOperationsTC[MatrixM[U]] {type MatrixDataElemT = Double}
      override type ContElemT = M#MatrixDataElemT

      override def max: Option[M#MatrixDataElemT] = ev.toArray(matrix).map(_.max)

      override def min: Option[M#MatrixDataElemT] = ev.toArray(matrix).map(_.min)
    }
  }
}
