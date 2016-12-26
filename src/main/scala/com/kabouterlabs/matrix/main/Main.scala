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

package com.kabouterlabs.matrix.main

import com.kabouterlabs.matrix.MatrixM
import com.kabouterlabs.matrix.MatrixOperations._
import spire.math._
import spire.implicits._
//import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
//import com.kabouterlabs.matrix.implicits.jeigen.JeigenDenseMatrixImplicit._
//import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaDenseMatrixImplicit._
import com.kabouterlabs.matrix.implicits.jblass.JblasDoubleMatrixImplicit._
//import com.kabouterlabs.matrix.implicits.apachecommonsmath.ApacheCommonsMathDenseMatrixImplicit._


object Use
{
    def run1() = {
      import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
      new MatrixExample
    }

    def run2() = {
      import com.kabouterlabs.matrix.implicits.jeigen.JeigenDenseMatrixImplicit._
      new MatrixExample
    }

    def run3() = {
      import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaDenseMatrixImplicit._
      new MatrixExample
    }

   def run4() = {
     import com.kabouterlabs.matrix.implicits.apachecommonsmath.ApacheCommonsMathDenseMatrixImplicit._

     new MatrixExample
   }

  def run5() = {
    import com.kabouterlabs.matrix.implicits.jblass.JblasDoubleMatrixImplicit._

    new MatrixExample
  }


}

object EigenTest {


  def run1() = {
    import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
    val mm1= matrix(3, 3, Array(-3.0, 1.0, -2.0, 0.0, -1.0, -1.0, 2.0, 0.0, 0.0))
    println(mm1)
    val eigr = mm1.eig
    val gh = for (e <- eigr) yield {
      for (col <- Range(0, e.eigenvalues.length)) yield for (row <- Range(0, e.eigenvalues.length)) yield {
        if (e.eigenvectorsComplex(col) == 0.0) Complex(e.eigenvectors(row, col), 0.0)
        else {
          val (offset1, offset2,factor) = if ((col % 2) == 0) (0,1, 1.0) else (-1,0, -1.0)
          Complex(e.eigenvectors(row, col + offset1), factor * e.eigenvectors(row, col + offset2))
        }
      }
    }

  println(gh)

//    val a2 = Array(23.0,67.0,-78.0,23.0,45.0,-65.0, 90.0,89.0, -102.0, -90.0,45.67,23.45,12.01,-1.0,-100.0,+67.0)
//        val hsize = math.sqrt(a2.length).toInt
//        val er  = Array(150.958023628061, -65.0496496682086, -7.61918697992639, -7.61918697992639)
//        val im  = Array(0.0, 0.0, 84.7958635197412 ,- 84.7958635197412)
//        val l3 = MatrixM(hsize, hsize, a2)
//   println(l3)
//    val eigr2 = l3.eig
//    for ( e <- eigr2) yield {
//      println(e.eigenvectors)
//      //println(e.eigenvalues)
//      //println(e.eigenvectorsComplex)
//      //val (rval,ival,_) = e
//      //      println(rval)
//      //      println(ival)
//      //      println(vec)
//    }
  }
  def run2() = {
    import com.kabouterlabs.matrix.implicits.jeigen.JeigenDenseMatrixImplicit._

    val mm1= matrix(3, 3, Array(-3.0, 1.0, -2.0, 0.0, -1.0, -1.0, 2.0, 0.0, 0.0))

    val eigr = mm1.eig
    println(eigr)
    println(eigr.vectors.map(_.mkString("\n")))

  }

  def run3() = {
    import com.kabouterlabs.matrix.implicits.jblass.JblasDoubleMatrixImplicit._

    val mm= matrix(3, 3, Array(-3.0, 1.0, -2.0, 0.0, -1.0, -1.0, 2.0, 0.0, 0.0))
    println(mm.stringefy)
    val eigr = mm.eig//eigen(mm1)

      println(eigr)
    val h = for (e <- eigr) yield {
      //println(e)
      e(0).print()
      val l = for (i <- Range(0, e(1).rows)) yield {val d = e(1).get(i,i); Complex(d.real(), d.imag())}
        println(l.mkString(","))
      val bdd = for (j <- Range(0, e(0).columns)) yield {
        (for (k <- Range(0, e(0).rows)) yield { val d = e(0).getColumn(j).get(k); Complex(d.real(), d.imag())}).toVector
      }
      println(bdd.mkString("\n"))
    }
//    //println(h.toArray.mkString(","))
//    val aaa = Array(-3.0, 1.0, -2.0, 0.0, -1.0, -1.0, 2.0, 0.0, 0.0)
//    //val z = for ( x <- a) yield (x,y)
//    val z = aaa.sliding(2,2)
//    for (t <- z) println(t.mkString(","))
//    //println("(complex) eigen values : " , eigr._1, "eigen vectors :", eigr._2)
//    for (e <- eigr) yield {
//      println(e._2(0).print())
//      println(e._2(0).data.mkString("\n"))
//
//    }
    println("==================")
    println(eigr.values.map(_.mkString(",")))
    println(eigr.vectors.map(_.mkString("\n")))
  }

  def run4() = {
    import com.kabouterlabs.matrix.implicits.apachecommonsmath.ApacheCommonsMathDenseMatrixImplicit._

    val mm= matrix(3, 3, Array(-3.0, 1.0, -2.0, 0.0, -1.0, -1.0, 2.0, 0.0, 0.0))
    //val mm= matrix(3, 3, Array(-3.0, 1.0, -2.0, 1.0, -1.0, -5.0, -2.0, -5.0, 7.0))
    println(mm.stringefy)
    val eigr = mm.eig//eigen(mm1)

    //println(eigr)
    val h = for (e <- eigr) yield {
      for (i <- Range(0, e.getImagEigenvalues.length, 1)) yield {
        if (e.getImagEigenvalue(i) == 0.0) {
          val rval = e.getEigenvector(i).toArray
          (for (r <- rval) yield {
            Complex(r, 0)
          }).toVector
        }
        else {
          if ((i % 2) == 0) {
            val rval = e.getEigenvector(i).toArray.zip(e.getEigenvector(i+1).toArray)
            (for ((real, imag) <- rval) yield {
              Complex(real, imag)
            }).toVector
          }
          else {
            val rval = e.getEigenvector(i-1).toArray.zip(e.getEigenvector(i).toArray)
            (for ((real, imag) <- rval) yield {
              Complex(real, -1 * imag)
            }).toVector
          }
        }

      }
    }
    println(h)
    //println(eigr.values.map(_.mkString(";")))
    //    //println(h.toArray.mkString(","))
    //    val aaa = Array(-3.0, 1.0, -2.0, 0.0, -1.0, -1.0, 2.0, 0.0, 0.0)
    //    //val z = for ( x <- a) yield (x,y)
    //    val z = aaa.sliding(2,2)
    //    for (t <- z) println(t.mkString(","))
    //    //println("(complex) eigen values : " , eigr._1, "eigen vectors :", eigr._2)
    //    for (e <- eigr) yield {
    //      println(e._2(0).print())
    //      println(e._2(0).data.mkString("\n"))
    //
    //    }
//    println("==================")
//    println(eigr.values.map(_.mkString(",")))
//    println(eigr.vectors.map(_.mkString("\n")))
  }
}



object Main extends App {
//  val a1 = Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0,90.0,33.0,107.0,-78.0,-23.0,14.0,33.0)
//  val a2 = Array(23.0,67.0,-78.0,23.0,45.0,-65.0, 90.0,89.0, -102.0, -90.0,45.67,23.45,12.01,-1.0,-100.0,+67.0)
//  val hsize = math.sqrt(a1.length).toInt
//  val lsize = math.sqrt(a2.length).toInt
//  val l3  = MatrixM(hsize, lsize, a1)
//  val result = -2.35969600000000e6
//   println(l3.stringefy)
//    l3.determinant.map(_ - result).map(scala.math.abs(_)).map(_ < 0.000001)
//
//   val det = for (a <- l3.eig.values) yield {
//     a.foldLeft(Complex(1.0,0.0))(_ * _ )
//   }
//  println(det)
  //:== ld).sum

//  val a1  = Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0,90.0,33.0,107.0,-78.0,-23.0,14.0,33.0)
//  val er1 = Array(81.3567339942231, 64.2735390962677, 7.28804710064899, -61.9183201911397)
//  val hsize = math.sqrt(a1.length).toInt
//  val l3 = MatrixM(hsize, hsize, a1)
//  val e = l3.eig.values
//
//  println(e.map(_.mkString(",")))
//
//  val v = for (a <- e) yield (a.map(_.imag).map(scala.math.abs)).sum
//  println(v)
//  //.map(scala.math.abs)).map(_.sum)
//
//          val fact = 100000
//          val s = er1.map(_ * fact).map(_.toInt)
//          val t = for (a <- e) yield a.map(_.real * fact).map(_.toInt)
//  println(t.map(_.mkString(",")))
//  println(s.mkString(","))
//         for (i <- s) {
//  //          assertResult((Some(true)), "value " + i + "not an eigenvalue") {
//              println(t.map(_.contains(i)))
//  //          }
//          }

//      val a1 = Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0, 90.0, 33.0, 107.0, -78.0, -23.0, 14.0, 33.0)
//      val hsize = math.sqrt(a1.length).toInt
//      val l3 = MatrixM(hsize, hsize, a1)
//      val eigvec = for (arr <- l3.eig.vectors) yield for ( e <- arr) yield MatrixM(e.length,1, {e.map(_.real).toArray})
//
//      val eigval = for (arr <- l3.eig.values)  yield for ( e <- arr) yield e.real
//
//
//    for (o <- eigvec.zip(eigval).map((tuple) => tuple._1.zip(tuple._2))) {
//      for ((evec, eval) <- o){
//        val res = (((l3 |* evec) :\ evec) :- MatrixM.fill(hsize, 1 , eval)).sum.map(_ < 0.0000001)
//        println(res)
//
//      }
//    }
////      for (veca <- eigvec) {
////        for (vec <- veca) {
////          val g:Array[Double] = vec.map(_.real).toArray
////          val eigen_vector = MatrixM(vec.length,1, {g})
////          val res = ((l3 |* eigen_vector) :\ eigen_vector)
////          println(res.stringefy)
////        }
////      }
//  println("=============")
//   println(eigval.map(_.mkString(",")))
//    println(eigvec.map(_.mkString("\n")))
//    println(r)
  //    for (i <- Range(0, hsize)) {
  //      assertResult(Some(true), "eigen vector /eigen value out of sync") {
  //        ((l3 |* ev(::, i)) :\ ev(::, i) :- MatrixM.fill(hsize, 1, e(i, 0).get)).sum.map(_ < 0.0000001)
  //      }
  //    }

  EigenTest.run2()
  EigenTest.run1()

//  val c1 = Complex(1.0,2.0)
//  val c2 = Complex(4.89, 56.78)
//  val c3 = c1 * c2
//  println(c3)
//
//  val r1 = Use.run1()
//  val r2 = Use.run2()
//  val r3 = Use.run3()
//  val r4 = Use.run4()
//  val r5 = Use.run5()
//  val mm = MatrixM.rand(5,5)
//  println(mm.stringefy)
//  println(mm.rows, mm.columns, mm.size, mm.isNull)
//  val s = for (m <- mm) yield m.toString
//  println(s.replace(";", "\n"))
  //r1()
  //r2()
  //r3()
  //r4()
  //r5()

/*
  val mm = MatrixM.rand(10,10)
  val mc = mm.deepCopy(2,1,89.90)
  deepCopy(mc)

  val o1 = mm(2,1)
  val o2 = mc(2,1)

  println(o1,o2, mm, mm(::,2))
  val mmd = MatrixM.rand(10,10)
  val ss = mmd(::,0 to 3)
  ss(2,1,909.890)
  println(mmd, ss)

  val l2 = rand(3, 3)
  val l3 = matrix(3, 3, Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0))
  val s1  = l2 :+ l3
  val s11 = add(l2,l3)
  val s2 = l2 :- l3
  val s22 = subtract(l2,l3)
  val s3 = l2 :\ l3
  val s4 = l2 :* l3
  val s5 = l2 |* l3

  val s1a = l2 ++ 7.0
  val s2a = l2 -- 7.0
  val s3a = l2 ** 7.0
  val s4a = l2 \\ 7.0

  val ta1 = s1 :== s1a
  val ta2 = s1 :<= s1a
  val ta3 = s1 :<<  s1a
  val ta4 = s1 :>>  s1a
  val ta5 = s1 :>=  s1a
  val ta6 = s1 :!=  s1a
  println(l2,l3)
  println("------------")
  println(s1,s5,s1a,s3a,ta5)

  val l5 = l2 concatDown l3
  l5(0,0,-90000)
  println(l2(0,0),l5(0,0))
  println(l2.toDiag)

  val mm0 = rand(3, 3)
  val mm1= matrix(3, 3, Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0))
  println("mm0", mm0, "mm1", mm1)
  println("mm1.inverse", mm1.inverse,"mm1.transpose", mm1.transpose, "mm1.determinant" ,mm1.determinant, mm0.trace)
  val res = mm1.solve(mm0)
  println("solving mm1 * res = mm0; res = ", res, "residual : ",(mm1 |* res) :- mm0 )
  val eigr = eigen(mm1)

  println("(complex) eigen values : " , eigr._1, "eigen vectors :", eigr._2)

  val eigm = mm0.eig
  EigenTest.run1()
  //EigenTest.run2()

   mm.csvWrite("/tmp/myfile.csv")
   val mr = csvRead("/tmp/myfile.csv")
  println(mm, mr)
  */
}


























