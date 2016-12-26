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

import java.io.{PrintWriter, StringWriter}


/**
  * Created by fons on 3/30/16.
  */
case class EigenResultM[U](result: Option[U])(implicit ev:EigenAccessT[U]) {
  type ElemT        = EigenAccessT[U]#ElemT
  type EigenValuesT = EigenAccessT[U]#EigenValuesT
  type EigenVectorT = EigenAccessT[U]#EigenVectorT

  private val valuesc  = ev.values(result)
  private val vectorsc = ev.vectors(result)
  def map[W](f:U=>W):W = result.map(f(_)).get

  def values:Option[EigenValuesT]  = valuesc
  def vectors:Option[EigenVectorT] = vectorsc
}


object EigenResultM {
  def none[U]()(implicit ev:EigenAccessT[U]) = new EigenResultM[U](None)

  def apply[U](f:  => U)(implicit ev:EigenAccessT[U]): EigenResultM[U] = {
    try {
      new EigenResultM[U](Some(f))
    }
    catch {
      case e: Throwable =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        new EigenResultM[U](None)
    }
  }
}