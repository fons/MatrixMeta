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

package com.kabouterlabs.matrix

/**
  * Created by fons on 12/30/16.
  */
object MatrixExtension {

  trait MatrixExtensionsTC[A]
  {
    type MatrixDataElemT
    def mapFunc(m:A, f:(MatrixDataElemT)=>MatrixDataElemT) :A
    def mapFilter(m:A, f:(Int,Int,MatrixDataElemT)=>MatrixDataElemT):A
    def reduceFunc(m:A, v: MatrixDataElemT, f: (MatrixDataElemT, MatrixDataElemT) => MatrixDataElemT):Option[MatrixDataElemT]
    def reduceFilter(m:A, v:MatrixDataElemT, f: (MatrixDataElemT, Int, Int, MatrixDataElemT) => MatrixDataElemT):Option[MatrixDataElemT]
    def foldFunc[W](m:A, w:W, f:(W,MatrixDataElemT)=>W):Option[W]
    def cos(m:A):A
    def acos(m:A):A
    def sin(m:A):A
    def asin(m:A):A
    def tan(m:A):A
    def atan(m:A):A
    def cosh(m:A):A
    def acosh(m:A):A
    def sinh(m:A):A
    def asinh(m:A):A
    def tanh(m:A):A
    def atanh(m:A):A
    def exp(m:A):A
    def log(m:A):A
    def log10(m:A):A
    def log1p(m:A):A
    def expm1(m:A):A
    def ceil(m:A):A
    def floor(m:A):A
    def abs(m:A):A
    def round(m:A):A
    def sqrt(m:A):A
    def pow(m:A, p:MatrixDataElemT):A
    def min(m:A):Option[MatrixDataElemT]
    def max(m:A):Option[MatrixDataElemT]
  }

  implicit class MatrixExtension$[A](m: A)(implicit ev: MatrixExtensionsTC[A]{type MatrixDataElemT=Double})
  {
    type MatrixElemT =  Double

    def mapFunc(f:(MatrixElemT)=>MatrixElemT) = ev.mapFunc(m, f)

    def mapFilter(f:(Int,Int,MatrixElemT)=>MatrixElemT):A = ev.mapFilter(m,f)

    def reduceFunc(v: MatrixElemT)(f: (MatrixElemT, MatrixElemT) => MatrixElemT):Option[MatrixElemT] = ev.reduceFunc(m,v,f)

    def reduceFilter(v:MatrixElemT)(f: (MatrixElemT, Int, Int, MatrixElemT) => MatrixElemT):Option[MatrixElemT] = ev.reduceFilter(m,v,f)

    def foldFunc[W](w:W)(f:(W,MatrixElemT)=>W):Option[W]   = ev.foldFunc(m,w,f)

    def cos:A    = ev.cos(m)
    def acos:A   = ev.acos(m)
    def sin:A    = ev.sin(m)
    def asin:A   = ev.asin(m)
    def tan:A    = ev.tan(m)
    def atan:A   = ev.atan(m)
    def cosh:A   = ev.cosh(m)
    def acosh:A  = ev.acosh(m)
    def sinh:A   = ev.sinh(m)
    def asinh:A  = ev.asinh(m)
    def tanh:A   = ev.tanh(m)
    def atanh:A  = ev.atanh(m)
    def exp:A    = ev.exp(m)
    def log:A    = ev.log(m)
    def log10:A  = ev.log10(m)
    def log1p:A  = ev.log1p(m)
    def expm1:A  = ev.expm1(m)
    def ceil:A   = ev.ceil(m)
    def floor:A  = ev.floor(m)
    def abs:A    = ev.abs(m)
    def round:A  = ev.round(m)
    def sqrt:A   = ev.sqrt(m)
    def pow(p:MatrixElemT):A = ev.pow(m, p)

    def max:Option[MatrixElemT] = ev.max(m)
    def min:Option[MatrixElemT] = ev.min(m)

  }

}
