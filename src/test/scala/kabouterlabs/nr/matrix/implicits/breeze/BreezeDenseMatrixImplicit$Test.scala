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

package test.kabouterlabs.nr.matrix.implicits.breeze

import com.kabouterlabs.matrix.MatrixM
import com.kabouterlabs.matrix.MatrixOperations._
import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
import org.scalatest._


/**
  * Created by fons on 4/17/16.
  */
class BreezeDenseMatrixImplicit$Test extends FlatSpec  with Matchers {

  val a1 = Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0,90.0,33.0,107.0,-78.0,-23.0,14.0,33.0)
  val a2 = Array(23.0,67.0,-78.0,23.0,45.0,-65.0, 90.0,89.0, -102.0, -90.0,45.67,23.45,12.01,-1.0,-100.0,+67.0)
  val hsize = math.sqrt(a1.length).toInt
  val lsize = math.sqrt(a2.length).toInt

  val t = Array[Double]( 4.0, 5.0, 6.0, 7.0, 8.0,21.0,
    56.0,-1.0,-9.0,67.0,45.0,89.0,
    23.0,67.0,-78.0,23.0,45.0,-65.0,
    90.0,89.0)

  val rows  = 6
  val colls = 3
  def throwAssert  {
    assert(1==2, "ok get a clue")
  }

  // Section 1 :Testing matrix creation
  "a matrix" should "be created" in {
    MatrixM(rows,colls,t)  should not equal None
  }

  //
  // Section * CompanionT as a way to create specialty matrices..
  //----------------------------------------------------------------------------------------------------

  it should "be able to create  an identy matrix" in {
    val l3  = MatrixM(hsize, lsize, a1)
    val idm = MatrixM.eye(hsize)

    assertResult(Some(a1.length.toDouble), "multiplying by the identity should result in the same matrix") {
      (l3 |* idm :== l3).sum
    }

    assertResult(Some(hsize), "the identity trace should be equal to size") {
      idm.sum
    }

  }

  it should "be able to create  a zero matrix" in {
    val l3  = MatrixM(hsize, lsize, a1)
    val zero = MatrixM.zero(hsize,lsize)

    assertResult(Some(a1.length.toDouble)) {
      (l3 :+ zero :== l3).sum
    }

  }

  it should "be able to create a diagonal matrix" in {
    val diag = MatrixM.diag(Array(1.0,1.0,1.0,1.0,1.0,1.0,1.0))
    val id   = MatrixM.eye(7)
    val zero  = MatrixM.zero(7,7)
    assertResult(Some(7.0*7.0)) {
      (id :- diag :== zero).sum
    }
  }

  it should "be able to create a small random sized matrix" in {
    MatrixM.rand(7,7) should not equal None

  }

  it should "be able to create a very large random sized matrix without exception" in {
    noException should be thrownBy MatrixM.rand(70000,7000)
  }

  it should "be able to handle out-of-bounds matrix creation" in {
    noException should be thrownBy MatrixM(hsize, lsize*1000, a1)
  }



  it should "be able to create a matrix of ones" in {
    assertResult(Some(5*5)) {
      MatrixM.one(5,5).sum
    }
    assertResult(Some(5*5)) {
      (MatrixM.one(5, 5) :== MatrixM.one(5, 5).transpose).sum
    }

  }



  it should "be able to create a matrix filled with a specific value" in {
    val sz = 66
    val l0 = MatrixM.one(sz,sz)
    val k0 = MatrixM.fill(1,sz,1.0/sz) concatDown MatrixM.fill(sz-1,sz,0)
    val r =l0 |* k0

  }

  // Section * Testing simple operations :MatrixOperationsTC
  //------------------------------------------------------------------------------------------------------
  //
  //
  it should "use the infix operator for element wise multiply" in {
    val l3 = MatrixM(hsize, lsize, a1)
    val l2 = MatrixM(hsize, lsize, a2)
    val r  = MatrixM(hsize, lsize, (a1,a2).zipped.map((l:Double, r:Double)=>l*r))
    assertResult(Some(a1.length.toDouble), "infix and function for element wise multiply op are not compatible") {
      (l3 :* l2 :== hadamard(l3, l2)).sum
    }
    assertResult(Some(a2.length.toDouble), "unexpected multiplication result") {
      (l3 :* l2 :== r).sum
    }

  }

  it should "use the infix operator for element wise sum" in {
    val l3 = MatrixM(hsize, lsize, a1)
    val l2 = MatrixM(hsize, lsize, a2)
    val r  = MatrixM(hsize, lsize, (a1,a2).zipped.map((l:Double, r:Double)=>l+r))
    assertResult(Some(a1.length.toDouble), "infix and function for element wise sum are not compatible") {
      (l3 :+ l2 :== add(l3, l2)).sum
    }
    assertResult(Some(a1.length.toDouble), "unexpected element wise sum result") {
      (l3 :+ l2 :== r).sum
    }
  }

  it should "use the infix operator for element wise subtract" in {
    val l3 = MatrixM(hsize, lsize, a1)
    val l2 = MatrixM(hsize, lsize, a2)
    val r  = MatrixM(hsize, lsize, (a1,a2).zipped.map((l:Double, r:Double)=>l-r))
    assertResult(Some(a1.length.toDouble), "infix and function for element wise sum are not compatible") {
      (l3 :- l2 :== subtract(l3, l2)).sum
    }
    assertResult(Some(a2.length.toDouble), "unexpected element wise subtractiontion result") {
      (l3 :- l2 :== r).sum
    }

  }

  it should "use the infix operator for element wise divide" in {
    val l3 = MatrixM(hsize, lsize, a1)
    val l2 = MatrixM(hsize, lsize, a2)
    val r  = MatrixM(hsize, lsize, (a1,a2).zipped.map((l:Double, r:Double)=>l/r))
    assertResult(Some(a1.length.toDouble), "infix and function for element wise sum are not compatible") {
      (l3 :\ l2 :== divide(l3, l2)).sum
    }
    assertResult(Some(a2.length.toDouble), "unexpected element wise devision result") {
      (l3 :\ l2 :== r).sum
    }
  }


  it should "element wise divide and multiply commute " in {
    val l3 = MatrixM(hsize, lsize, a1)
    val l2 = MatrixM(hsize, lsize, a2)

    assertResult(Some(a1.length.toDouble), "infix and function for element wise sum are not compatible") {
      ((l3 :\ l2) :* l2 :== hadamard(l2, divide(l3, l2))).sum
    }

    assertResult(Some(false), "element wise divide and mult don't commute") {
      (l2 :* l3 :\ l2 :- l3).sum.map(_ > 0.01)
    }
  }


  it should "use the infix operator for matrix multiply" in {
    val l3 = MatrixM(hsize, lsize, a1)
    val l2 = MatrixM(hsize, lsize, a2)
    val arr = Array(-464.0,-6027.0,1638.0,-7493.0,-8092.0,4913.0,846.0,12947.0,-3368.13,1170.9500000000003,
      -3816.5899999999992,5036.540000000001,-4285.96,-10501.95,-2345.94,-8403.93)
    val r  = MatrixM(hsize, lsize, arr)
    assertResult(Some(a1.length.toDouble), "infix and function for matrix multiply are not compatible") {
      (l3 |* l2 :== multiply(l3, l2)).sum
    }
    assertResult(Some(a2.length.toDouble), "unexpected matrix multiply result") {
      (l3 |* l2 :== r).sum
    }

  }

  // Section * Testing simple operations :MatrixOpsByElementT
  //           apply a single value to a all elements of a maytrix
  //------------------------------------------------------------------------------------------------------
  //
  //
  it should "use the infix operator for multiply by value" in {
    val lm = MatrixM(hsize, lsize, a1)
    val lv = 67.34
    val r  = MatrixM(hsize, lsize, a1.map(_ * lv))

    assertResult(Some(a1.length.toDouble), "infix and function for value multiply op are not compatible") {
      (lm ** lv :== multiply1(lm, lv)).sum
    }

    assertResult(Some(a2.length.toDouble), "unexpected value multiplication result") {
      (lm ** lv :== r).sum
    }

  }

  it should "use the infix operator for divide with value" in {
    val lm = MatrixM(hsize, lsize, a1)
    val lv = 67.34
    val r  = MatrixM(hsize, lsize, a1.map(_ / lv))

    assertResult(Some(a1.length.toDouble), "infix and function for value divide are not compatible") {
      (lm \\ lv :== divide1(lm, lv)).sum
    }
     //TODO : sync up with eigen test

    assertResult(Some(true), "unexpected division result") {
      (lm \\ lv :- r).sum.map(_ < 0.0000001)
    }

  }


  it should "use the infix operator for sum with value" in {
    val lm = MatrixM(hsize, lsize, a1)
    val lv = 67.34
    val r  = MatrixM(hsize, lsize, a1.map(_ + lv))

    assertResult(Some(a1.length.toDouble), "infix and function for value sum are not compatible") {
      (lm ++ lv :== add1(lm, lv)).sum
    }

    assertResult(Some(a2.length.toDouble), "unexpected summation result") {
      (lm ++ lv :== r).sum
    }

  }


  it should "use the infix operator for subtract with value" in {
    val lm = MatrixM(hsize, lsize, a1)
    val lv = 67.34
    val r  = MatrixM(hsize, lsize, a1.map(_ - lv))

    assertResult(Some(a1.length.toDouble), "infix and function for value subtract are not compatible") {
      (lm -- lv :== subtract1(lm, lv)).sum
    }

    assertResult(Some(a2.length.toDouble), "unexpected summation result") {
      (lm -- lv :== r).sum
    }

  }

  it should "commute divide and mutiply with value" in {
    val lm = MatrixM(hsize, lsize, a1)
    val lv = 67.34
    //TODO : sync with eigen test
    assertResult(Some(true), "divide1 and multiply1 should commute ") {
      (((lm \\ lv) ** lv) :- divide1(multiply1(lm, lv),lv)).sum.map(_ < 0.000001)
    }
    //TODO : sync with eigen test
    assertResult(Some(true), "unexpected divide1/multiply1 commute result") {
      (lm \\ lv ** lv :- lm).sum.map(_ < 0.000001)
    }

  }

  // Section * Testing  by element compare operations :   MatrixCompareOpsByElementT
  //
  //------------------------------------------------------------------------------------------------------
  //
  //
  it should "use the infix operator for matrix equality" in {
    val lm = MatrixM(hsize, lsize, a1)
    assertResult(Some(a1.length.toDouble), "infix and function for matrix equality are not compatible") {
      (lm :== lm :== mEqual(lm,lm)).sum
    }
    assertResult(Some(a1.length.toDouble), "unexpected value matrix equality result") {
      (lm :== lm).sum
    }

    assertResult(Some(a1.length.toDouble), "unexpected value matrix larger or equal when equal result") {
      (lm :>= lm).sum
    }

    assertResult(Some(a1.length.toDouble), "unexpected value matrix smaller or equal when equal result") {
      (lm :<= lm).sum
    }
  }


  it should "use the infix operator for matrix in-equality" in {
    val lm = MatrixM(hsize, lsize, a1)
    assertResult(Some(a1.length.toDouble), "infix and function for matrix inequality are not compatible") {
      (lm :!= (lm ** 0.234) :== mNotEqual(lm,lm ** 0.234)).sum
    }

    assertResult(Some(a1.length.toDouble), "unexpected value matrix in equality result") {
      (lm :!= (lm ** 1.34)).sum
    }

  }

  it should "use the infix operator for matrix smaller or equal " in {
    val lm = MatrixM(hsize, lsize, a1)
    assertResult(Some(a1.length.toDouble), "infix and function for matrix smaller or equal are not compatible") {
      (lm :<= (lm ** 1.234) :== mSmallerEqual(lm, lm ** 1.234)).sum
    }

    assertResult(Some(a1.length.toDouble), "infix and function for matrix smaller than are not compatible") {
      (lm :<< (lm ** 1.234) :== mSmaller(lm, lm ** 1.234)).sum
    }

    assertResult(Some(a1.filter(_>= 0).length.toDouble), "unexpected value matrix smaller /equal result") {
      (lm :<= (lm ** 1.34)).sum
    }

    assertResult(Some(a1.filter(_ > 0).length.toDouble), "unexpected value matrix smaller result") {
      (lm :<< (lm ** 1.34)).sum
    }
  }

  it should "use the infix operator for matrix larger or equal " in {
    val lm = MatrixM(hsize, lsize, a1)
    assertResult(Some(a1.length.toDouble), "infix and function for matrix smaller or equal are not compatible") {
      (lm :>= (lm ** 0.234) :== mGreaterEqual(lm,lm ** 0.234)).sum
    }

    assertResult(Some(a1.length.toDouble), "infix and function for matrix smaller than are not compatible") {
      (lm :>> (lm ** 0.234) :== mGreater(lm,lm ** 0.234)).sum
    }

    assertResult(Some(a1.filter(_ >= 0).length.toDouble), "unexpected value matrix smaller /equal result") {
      (lm :>= (lm ** 0.34)).sum
    }

    assertResult(Some(a1.filter(_> 0).length.toDouble), "unexpected value matrix smaller result") {
      (lm :>> (lm ** 0.34)).sum
    }

  }


  // Section * Testing  matrix slicing : SliceT
  //
  //------------------------------------------------------------------------------------------------------
  //


  it should "be able to extract an array and convert back to the same matrix" in  {
    val r = Array[Double](434.00000, 417.00000,  -489.00000,  501.00000,   527.00000,   139.00000,
      959.00000,  1434.00000,  -1668.00000,   1068.00000,   1361.00000,   -506.00000,
      -39.00000, -322.00000, 1047.00000, 118.00000, -2.00000, 1672.00000)
    val k1 = MatrixM(3,3,r)
    val k2 = MatrixM(3,3,k1.toArray)
    assertResult(Some(9.0)) {
      (k1 :== k2).sum
    }
  }

  it should "be able to be able to concatenate an array to the right" in  {
    val r = Array[Double](434.00000, 417.00000,  -489.00000,  501.00000,   527.00000,   139.00000,
      959.00000,  1434.00000,  -1668.00000,   1068.00000,   1361.00000,   -506.00000,
      -39.00000,
      -322.00000,
      1047.00000,
      118.00000,
      -2.00000,
      1672.00000)


    val l2 = MatrixM(3,3,r )
    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
    val l4 = l2 concatRight l3
    assertResult(Some(18.0), "unable to concat to the right") {
      val a = (l4(::, 0 to 2) :== l2).sum
      val b = (l4(::, 3 to 5) :== l3).sum
      (a,b) match {
        case (Some(l), Some(r))=>Some(l+r)
        case _ => None
      }
    }

  }

  it should "be able to be able to concatenate an array at the bottom" in  {
    val r = Array[Double](434.00000, 417.00000,  -489.00000,  501.00000,   527.00000,   139.00000,
      959.00000,  1434.00000,  -1668.00000,   1068.00000,   1361.00000,   -506.00000,
      -39.00000,
      -322.00000,
      1047.00000,
      118.00000,
      -2.00000,
      1672.00000)
    val l2 = MatrixM(3,3,r )
    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
    val l4 = l2 concatDown  l3
    assertResult(Some(18.0)) {
      val a = (l4(0 to 2, ::) :== l2).sum
      val b = (l4(3 to 5, ::) :== l3).sum
      (a,b) match {
        case (Some(l), Some(r))=>Some(l+r)
        case _ => None
      }
    }

  }

  it should "be able to be able to extract a diagonal marix" in  {
    val l0 = MatrixM(hsize, hsize, a1)
    val id = MatrixM.eye(hsize)
    val ld = l0.toDiag
    val s = (for (i <- Range(0, hsize)) yield {l0(i, i)})
    def |+|(l:Option[Double], r:Option[Double]) : Option[Double] = for ( x<- l; y <- r) yield (x+y)

    val s1 = s.foldLeft[Option[Double]](Some(0.0))(|+|(_,_))

    assertResult(s1) {
      ld.sum
    }
    assertResult(Some(hsize*hsize.toDouble)) {
      (l0 :* id :== ld).sum
    }

  }


  it should "not be able to extract a diagonal marix from a non-square matrix" in  {
    val l0 = MatrixM(hsize, hsize, a1)
    val id = MatrixM.eye(hsize)
    val ld = l0.toDiag
    val s = (for (i <- Range(0, hsize)) yield {l0(i, i)})
    def |+|(l:Option[Double], r:Option[Double]) : Option[Double] = for ( x<- l; y <- r) yield (x+y)

    val s1 = s.foldLeft[Option[Double]](Some(0.0))(|+|(_,_))

    assertResult(s1) {
      ld.sum
    }
    assertResult(Some(hsize*hsize.toDouble)) {
      (l0 :* id :== ld).sum
    }
  }

  it should "be able to set a value" in {
    val z = MatrixM.zero(4,4)
    z(0,0,-56.0)
    assertResult(Some(-56)){
      z(0,0)
    }
  }

  it should "be able to create a deep copy of a matrix" in {
    val l0 = MatrixM.rand(hsize,hsize)
    val l2 = l0.deepCopy

    assertResult(Some(hsize*hsize)) {
      (l0 :== l2).sum
    }

  }

  it should "a deep copy of a matrix can be changed and it will not chnage the original" in {
    val l0 = MatrixM.rand(hsize,hsize)
    val l2 = l0.deepCopy

    assertResult(Some(true)) {for (c1 <-l0(1,3); c2 <- l2(1,3)) yield c1 == c2}

    l2(1,3,78.90)
    assertResult(Some(true)) {for (c1 <-l0(1,3); c2 <- l2(1,3)) yield (c1 != c2) && (c2 == 78.90)}

    assertResult(Some(hsize*hsize - 1)) {
      (l0 :== l2).sum
    }

  }

  //
  // Section  Aggregation Test : AggregateT
  //------------------------------------------------------------------------------------------------------
  //
  it should "calculate the trace of a simple matrix" in {
    val l3 = MatrixM(3, 3, Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0))

    assertResult(Some(3.0)) {
      l3.trace
    }
  }

  it should "sum over all the elements of a simple matrix" in {
    assertResult(Some(a1.sum)) {
      MatrixM(hsize, lsize, a1).sum
    }
  }

  it should "sum over all the rows of a simple matrix" in {
    val r = Array[Double](434.00000, 417.00000,  -489.00000,  501.00000,   527.00000,   139.00000,
      959.00000,  1434.00000,  -1668.00000,   1068.00000,   1361.00000,   -506.00000,
      -39.00000, -322.00000, 1047.00000, 118.00000, -2.00000, 1672.00000)
    val t = Array[Double](434.00000 + 417.00000 -489.00000,
      501.00000 + 527.00000  + 139.00000,
      959.00000 + 1434.00000 +  -1668.00000)

    assertResult(Some(3.0)) {
      (MatrixM(3, 3, r).sumRows :== MatrixM(1, 3, t)).sum
    }

  }

  it should "sum over all the columns of a simple matrix" in {
    val r = Array[Double](434.00000, 417.00000,  -489.00000,  501.00000,   527.00000,   139.00000,
      959.00000,  1434.00000,  -1668.00000,   1068.00000,   1361.00000,   -506.00000,
      -39.00000, -322.00000, 1047.00000, 118.00000, -2.00000, 1672.00000)
    val t = Array[Double](434.00000 + 501.00000 + 959.00000,
      417.00000 + 527.00000 + 1434.00000,
      -489.00000 + 139.00000 -1668.00000)

    assertResult(Some(3.0)) {

      (MatrixM(3, 3, r).sumCols :== MatrixM(3, 1, t)).sum
    }

  }

  //
  // Section * Testing Linear Algebra
  //-----------
  // -------------------------------------------------------------------------------------------------
  //

  it should "be able to get the inverse matrix" in   {
    val l0 = MatrixM(hsize, hsize, a1)
    val l1 = l0.inverse
    assertResult(Some(true), "matrix times inverse should equal id ") {
      ((l0 |* l1) :- MatrixM.eye(hsize)).sum.map(_ < 0.00000009)
    }
    val k1 = MatrixM(1,4, Array(0.133383283270387,-0.193441867087964,0.0562818261335358,0.156569320793865))
    val k2 = k1 concatDown MatrixM(1,4, Array(-0.00868671218665456,0.0138975529051200,0.0135898861548267, -0.0166114618154203))
    val k3 = k2 concatDown MatrixM(1,4, Array(-0.00693479160027393,   0.0159732440110930, -0.00505488842630576 , -0.00311396044236207))
    val k4 = k3 concatDown MatrixM(1,4, Array(-0.00607112102575925,  -0.0103377723232145,  0.00486333832832704, 0.00668475939273533))
    assertResult(Some(true), "shuould get the actual inverse") {
      (l1 :- k4).sum.map(_ < 0.000000009 )
    }
  }

  it should "not be able to invert a singular matrix" in {

    val m = MatrixM.one(5,5)
    val i = m.inverse
    i.matrix match {
      case Some(m) => assert(1==2,"cannot invert singular matrix")
      case None => assert(1==1)
    }

  }

  it should "be able to invert a large matrix " in {
    assertResult(Some(true), "unable to invert a reasonbly large matrix") {
      val sz = 250
      val l0 = MatrixM.one(sz,sz)  :- MatrixM.eye(sz)
      val k0 = l0.inverse
      val r = k0(0,::).toArray.map(_.sum)
      r.map(1.0 / _).map(_ - (sz-1)).map(_ < 0.0000001)
    }
  }

  it should "be able to solve a set of equations " in {
    val l0 = MatrixM(hsize, hsize, a1)
    val x  = MatrixM(hsize, 3, Array(1.0,2.0,3.0,4.0,0.4,0.3,0.56,0.489,10,45,900,-90))

    val r  = l0.solve(x)
    assertResult(Some(true)) {
      ((l0 |* r) :- x).sum.map(_ < 0.00000000001)
    }

  }


  it should "be able to transpose a matrix" in {
    val r = 100
    val c = 120
    val l0 = MatrixM.rand(r, c)
    val l1 = l0.transpose
    for (i <- Range(0,r)) {
      assertResult(Some(c), "unable to transpose a matrix") {
        (l0(i,::) :== l1(::,i).transpose).sum
      }
    }
  }

  it should "be able to get the determinant" in {
    val l3  = MatrixM(hsize, lsize, a1)
    val result = -2.35969600000000e6
    assertResult(Some(true)) {
      l3.determinant.map(_ - result).map(scala.math.abs(_)).map(_ < 0.000001)
    }

  }

  it should "be able to get the eigenvalues" in {
    val a1 = Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0, 90.0, 33.0, 107.0, -78.0, -23.0, 14.0, 33.0)
    val er1 = Array(81.3567339942231, 64.2735390962677, 7.28804710064899, -61.9183201911397)
    val hsize = math.sqrt(a1.length).toInt
    val l3 = MatrixM(hsize, hsize, a1)
    val e = l3.eig.values


    assertResult(Some(0), "imaginary parts not zero") {
      for (a <- e) yield (a.map(_.imag).map(scala.math.abs)).sum
    }
    val fact = 100000
    val s = er1.map(_ * fact).map(_.toInt)

    val t = for (a <- e) yield a.map(_.real * fact).map(_.toInt)
    for (i <- s) {
      assertResult((Some(true)), "value " + i + "not an eigenvalue") {
        t.map(_.contains(i))
      }
    }


  }

  it should "be able to get the eigenvalues with imaginary parts" in {

    val a2 = Array(23.0, 67.0, -78.0, 23.0, 45.0, -65.0, 90.0, 89.0, -102.0, -90.0, 45.67, 23.45, 12.01, -1.0, -100.0, +67.0)
    val hsize = math.sqrt(a2.length).toInt
    val er = Array(150.958023628061, -65.0496496682086, -7.61918697992639, -7.61918697992639)
    val im = Array(0.0, 0.0, 84.7958635197412, -84.7958635197412)
    val l3 = MatrixM(hsize, hsize, a2)
    val e = l3.eig.values

    val value = im.map(scala.math.abs(_)).foldLeft(0.0)(_ + _)
    assertResult(Some(true), "imaginary parts are zero") {
      for (a <- e) yield scala.math.abs(((a.map(_.imag).map(scala.math.abs)).sum - value)) < 0.0000000001
    }

    val fact = 100000

    val t = for (a <- e) yield a.map(_.real * fact).map(_.toInt)
    for (i <- er.map(_ * fact).map(_.toInt)) {
      assertResult((Some(true)), "value " + i + "not a real eigenvalue") {
        t.map(_.contains(i))
      }
    }

    val y = for (a <- e) yield a.map(_.imag * fact).map(_.toInt)
    for (i <- im.map(_ * fact).map(_.toInt)) {
      assertResult((Some(true)), "value " + i + " not an imaginary eigenvalue in " + y.map(_.mkString(","))) {
        y.map(_.contains(i))
      }
    }

  }


  it should "be able to get the eigenvectors" in {
    val a1 = Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0, 90.0, 33.0, 107.0, -78.0, -23.0, 14.0, 33.0)
    val hsize = math.sqrt(a1.length).toInt
    val l3 = MatrixM(hsize, hsize, a1)
    val eigvec = for (arr <- l3.eig.vectors) yield for (e <- arr) yield MatrixM(e.length, 1, {
      e.map(_.real).toArray
    })

    val eigval = for (arr <- l3.eig.values) yield for (e <- arr) yield e.real

    for (o <- eigvec.zip(eigval).map((tuple) => tuple._1.zip(tuple._2))) {
      for ((evec, eval) <- o) {
        assertResult(Some(true), "eigen vector /eigen value out of sync") {
          (((l3 |* evec) :\ evec) :- MatrixM.fill(hsize, 1, eval)).sum.map(_ < 0.0000001)
        }
      }
    }

  }

  //
  // Section * SerializeT
  //-----------
  // -------------------------------------------------------------------------------------------------
  //

  it should "be able to serialize to a csv file" in {
    val m = MatrixM.rand(1000,1000)
    val fn = "/tmp/testserializebreeze.csv"
    MatrixM.csvwrite(fn, m)
    val k = MatrixM.csvread(fn)
    val res = m :- k

    assertResult(Some(0.0), "unable to (de) serialize csv file") {
      res.sum
    }

  }




}
