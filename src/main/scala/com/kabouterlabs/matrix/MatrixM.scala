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



case class MatrixM[V](matrix: Option[V]) {

  override def toString = matrix match {
    case Some(m) =>  "{" + m.getClass.getName + "\n" + m.toString + "}"
    case None    => "{none}"
  }

  def safeMap[W](f: V => W): Option[W] = {
    try {
      matrix.map(f(_))
    }
    catch {
      case e: Throwable =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        None
    }
  }

  def map1[W](f:V=>W) : MatrixM[W] = {
    matrix match {
      case Some(matrixm) => MatrixM(matrix.map(f(_)))
      case None => MatrixM.none
    }
  }
  def map[W](f:V=>W):W = matrix.map(f(_)).get

  def flatMap[W](f:V=>MatrixM[W]):MatrixM[W] = {
    matrix match {
      case None => MatrixM.none
      case Some(_matrix_) => {
        try {
          f(_matrix_)
        }
        catch {
          case e: Throwable =>
            val sw = new StringWriter
            e.printStackTrace(new PrintWriter(sw))
            println("exception caught :" + e + sw)
            MatrixM.none
        }
      }
    }
  }
}

object MatrixM {


  def apply(row: Int, col: Int, data: => Array[Double])(implicit factory: FactoryT) = safeMap2[factory.MatrixImplT](()=>{factory(row, col, data)})

  def apply(row: Int, col: Int)(implicit factory: FactoryT) = safeMap2[factory.MatrixImplT](()=>{factory(row, col)})

  def apply(row: Int, col: Int, data: Option[Array[Double]])(implicit factory: FactoryT) = {
    data match {
      case Some(_data_) => MatrixM[factory.MatrixImplT]({factory(row, col, _data_)})
      case None => MatrixM[factory.MatrixImplT]({None})
    }
  }


  def zero(row: Int, colls: Int)(implicit companion: CompanionT): companion.MatrixImplT = companion.zeros(row, colls)

  def eye(size: Int)(implicit companion: CompanionT): companion.MatrixImplT = companion.eye(size)

  def rand(row: Int, colls: Int)(implicit companion: CompanionT): companion.MatrixImplT = companion.rand(row, colls)

  def diag(data: Array[Double])(implicit companion: CompanionT): companion.MatrixImplT = companion.diag(data)

  def one(row: Int, col: Int)(implicit companion: CompanionT): companion.MatrixImplT = companion.ones(row, col)

  def fill(row: Int, col: Int, value: Double)(implicit companion: CompanionT): companion.MatrixImplT = companion.fill(row, col, value)

  def csvwrite[U](fn: String, u: U)(implicit serializeT: SerializeT[U]): Unit = serializeT.csvWrite(fn, u)

  def csvread[U](fn: String)(implicit serializeT: SerializeT[U]) = serializeT.csvRead(fn)

  def <=[U](fn: String)(implicit serializeT: SerializeT[U]) = serializeT.csvRead(fn)

  def none[U] = new MatrixM[U](None)


  def apply[U](f: => U): MatrixM[U] = {
    try {
      new MatrixM[U](Some(f))
    }
    catch {
      case e: Throwable =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        new MatrixM[U](None)
    }
  }

  def safeMap[U](f: ()  => U): MatrixM[U] = {
      try {
        new MatrixM[U](Some(f()))
      }
      catch {
        case e: Throwable =>
          val sw = new StringWriter
          e.printStackTrace(new PrintWriter(sw))
          println("exception caught :" + e + sw)
          new MatrixM[U](None)
      }

    }

  private def safeMap2[U](f: ()  => Option[U]): MatrixM[U] = {
    try {
      new MatrixM[U](f())
    }
    catch {
      case e: Throwable =>
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        new MatrixM[U](None)
    }

  }
}

