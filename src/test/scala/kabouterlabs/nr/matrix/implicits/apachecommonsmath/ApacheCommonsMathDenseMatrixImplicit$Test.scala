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

package test.kabouterlabs.nr.matrix.implicits.apachecommonsmath

/**
  * Created by fons on 12/15/16.
  */
import com.kabouterlabs.matrix.MatrixM
import com.kabouterlabs.matrix.MatrixOperations._
import com.kabouterlabs.matrix.implicits.apachecommonsmath.ApacheCommonsMathDenseMatrixImplicit._
import org.scalatest._

class ApacheCommonsMathDenseMatrixImplicit$Test extends FlatSpec  with Matchers {
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
  def throwAssert = {
    assert(1==2, "ok get a clue")
  }

  // Section 1 :Testing matrix creation
  "a matrix" should "be created" in {
    MatrixM(rows,colls,t)  should not equal None
  }

  it should "be able to extract and recreate the same matrix " in {

    (MatrixM(rows,colls,t) :== MatrixM(rows,colls, MatrixM(rows,colls,t).toArray)).sum
  }
  //
  // Section * CompanionT as a way to create specialty matrices..
  //----------------------------------------------------------------------------------------------------

  it should "be able to create  an identy matrix" in {
    val l3  = MatrixM(hsize, lsize, a1)
    val idm = MatrixM.eye(hsize)

    assertResult(Some(a1.length.toDouble), "multiplying by the identignorey should result in the same matrix") {
      (l3 |* idm :== l3).sum
    }

    assertResult(Some(hsize), "the identignorey trace should be equal to size") {
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
    val s = for (i <- Range(0, hsize)) yield l0(i, i)
    def |+|(l:Option[Double], r:Option[Double]) : Option[Double] = for ( x<- l; y <- r) yield x+y

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

  it should "a deep copy of a matrix can be changed and ignore will not chnage the original" in {
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

  it should "be able to serialize to a small csv file" in {
    val m = MatrixM.rand(10,10)
    val fn = "/tmp/testserializeapachecommonsmath.csv"
    MatrixM.csvwrite(fn, m)
    val k = MatrixM.csvread(fn)
    val res = m :- k

    assertResult(Some(0.0), "unable to (de) serialize csv file") {
      res.sum
    }

  }
  //
  // Section * SingularValueDecompostionT
  //----------------------------------------
  //----------------------------------------
  //

  it should "be able to svd a 2x3 matrix; fundamental invariants" in {
    val accuracy = 1.0E-8

    val arr = Array(3.0, 2.0,2.0,3.0,2.0,-2.0)
    val mat  = MatrixM(2,3,arr)
    val res = mat.svd
    assertResult(Some(true), " Vt not orthogonal") {
      ((res.Vt |* res.Vt.transpose) :- MatrixM.eye(2)).sum.map(scala.math.abs).map(_ < accuracy)
    }

    assertResult(Some(true), " U not orthogonal") {
      ((res.U.transpose |* res.U) :- MatrixM.eye(2)).sum.map(scala.math.abs).map(_ < accuracy)
    }

    assertResult(Some(true), "ssvd of A doesn't yield A back") {
      ((res.U |* res.Sm() |* res.Vt) :- mat).sum.map(scala.math.abs).map(_ < accuracy)
    }


  }

  ignore should "be able to svd a 2x3 matrix; wide value format" in {
    val accuracy = 1.0E-8

    val arr = Array(3.0, 2.0,2.0,3.0,2.0,-2.0)
    val mat  = MatrixM(2,3,arr)
    val res = mat.svd


    val a = 1.0/scala.math.sqrt(2.0)
    val b = 1.0/scala.math.sqrt(18.0)

    val arrU = Array(-a, -a, -a, a)
    val U = MatrixM(2,2,arrU)

    assertResult(Some(4.0), "U value not correct") {
      val dm  = res.U :\ U
      val fac = dm(0,0)
      val dm2 = dm \\ fac.get
      dm2.sum.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5))
    }
    val sentinel = 1.0 //this is in fact 0; but you can't divide by 0 !)
    val arrVt = Array(-a, -b, -2.0/3.0, -a , b, 2.0/3.0, sentinel, -4.0 * b, 1.0/3.0)
    val Vt    = MatrixM(3,3,arrVt)

    assertResult(Some(9.0 - sentinel), "Vt value not correct") {
      val dm  = res.Vt :\ Vt
      val fac = dm(0,0)
      val dm2 = dm \\ fac.get
      dm2.sum.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5))
    }

    val arrS = Array(5.0,0.0,0.0,3.0,0.0,0.0)
    val S    = MatrixM(2,3,arrS)
    assertResult(Some(true), "U value not correct") {
      val dm  = res.Sm :- S
      dm.sum.map(scala.math.abs).map(_ < accuracy)
    }

  }

  //===
  it should "svd should work  without throwing an exception for null matrices" in {
    noException should be thrownBy {
      val arr = Array(2.0,1.0,5.0,7.0,0.0, 0.0,6.0,0.0,0.0,10.0, 8.0,0.0,7.0,8.0,0.0,  6.0,1.0,4.0,5.0,0.0 ,0.0,7.0,0.0,0.0,7.0)
      val mat  = MatrixM(5,6,arr)
      val res= mat.svd
      assertResult(MatrixM.none, "Matrix U not Non") {
        res.U
      }
      assertResult(MatrixM.none, "Matrix Vt not Non") {
        res.Vt
      }
      assertResult(None, "Array S not Non") {
        res.S
      }
      assertResult(MatrixM.none, "Matrix Sm not Non") {
        res.Sm
      }
    }
  }

  it should "be able to svd a 5x5 matrix; fundamental invariants" in {
    val accuracy = 1.0E-8

    val arr = Array(2.0,1.0,5.0,7.0,0.0, 0.0,6.0,0.0,0.0,10.0, 8.0,0.0,7.0,8.0,0.0,  6.0,1.0,4.0,5.0,0.0 ,0.0,7.0,0.0,0.0,7.0)
    val mat  = MatrixM(5,5,arr)
    val res = mat.svd
    assertResult(Some(true), " Vt not orthogonal") {
      ((res.Vt.transpose |* res.Vt) :- MatrixM.eye(5)).sum.map(scala.math.abs).map(_ < accuracy)
    }

    assertResult(Some(true), " U not orthogonal") {
      ((res.U.transpose |* res.U) :- MatrixM.eye(5)).sum.map(scala.math.abs).map(_ < accuracy)
    }

    assertResult(Some(true), "svd of A doesn't yield A back") {
      ((res.U |* res.Sm() |* res.Vt) :- mat).sum.map(scala.math.abs).map(_ < accuracy)
    }


  }

  it should  "match the values (up to a factor) of the svd example" in {
    val arr = Array(2.0, 1.0, 5.0, 7.0, 0.0, 0.0, 6.0, 0.0, 0.0, 10.0, 8.0, 0.0, 7.0, 8.0, 0.0, 6.0, 1.0, 4.0, 5.0, 0.0, 0.0, 7.0, 0.0, 0.0, 7.0)
    val mat = MatrixM(5, 5, arr)
    val Uarr = Array(-0.54225536,
      -0.10181247,
      -0.52495325,
      -0.64487038,
      -0.06449519,
      0.06499573,
      -0.59346055,
      0.05938171,
      0.07040626,
      -0.79692967,
      0.82161708,
      -0.11255162,
      -0.21296861,
      -0.50874368,
      0.09000966,
      0.10574661,
      0.78812338,
      -0.11574223,
      -0.05990271,
      -0.59219473,
      -0.12448979,
      0.06026999,
      0.81372354,
      -0.56282918,
      -0.04412631)
    val U = MatrixM(5, 5, Uarr)

    val Varr = Array(
      -0.46461713,
      -0.07008599,
      -0.73509354,
      -0.48439167,
      -0.06496983,
      0.02150651,
      -0.75998796,
      0.09879712,
      0.0254474,
      -0.64151954,
      -0.86850856,
      0.06307148,
      0.28400852,
      0.39886566,
      -0.04427431,
      0.00079955,
      -0.60134567,
      -0.22348457,
      0.33268381,
      0.69120104,
      -0.17134943,
      -0.22784122,
      0.5650402,
      -0.70352314,
      0.32328395

    )
    val V = MatrixM(5, 5, Varr)
    val Vt = V.transpose

    val Sarr = Array(17.91837086, 15.17137188, 3.56400204, 1.98422815, 0.34955567)
    val res = mat.svd
    assertResult(Some(0), "Vt doesn't match ; mutiple of 5 test") {
      (res.Vt :\ Vt).sum.map((x) => scala.math.floor(x + 0.5) % 5)
    }
    assertResult(25.0, "Vt doesn't match ;  25 sum test") {
      scala.math.floor(0.5 + ((res.Vt :\ Vt).toArray).get.map(scala.math.abs).sum)
    }

    assertResult(Some(0), "U doesn't match ; mutiple of 5 test") {
      (res.U :\ U).sum.map((x) => scala.math.floor(x + 0.5) % 5)
    }
    assertResult(25.0, "U doesn't match ;  25 sum test") {
      scala.math.floor(0.5 + ((res.U :\ U).toArray).get.map(scala.math.abs).sum)
    }
    assertResult(Some(0), "Singualr values  don't match ") {
      res.S.map(_.zip(Sarr).map((x) => x._1 - x._2).sum).map(scala.math.abs).map((x) => scala.math.floor(x + 0.5))
    }
  }
  //
  // Section * QRDecompostionT
  //----------------------------------------
  //----------------------------------------
  //

  it should "take the qr decompostion of a 5x5 matrix: fundamental invariants" in {
    val arr = Array(2.0,1.0,5.0,7.0,0.0, 0.0,6.0,0.0,0.0,10.0, 8.0,0.0,7.0,8.0,0.0,  6.0,1.0,4.0,5.0,0.0 ,0.0,7.0,0.0,0.0,7.0)
    val mat  = MatrixM(5,5,arr)

    val res = mat.qr


    assertResult(Some(25.0), "Q matrix is not orthogonal") {
      val itest = res.Q |* res.Q.transpose
      val norm  = itest.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(5,5,norm) :== MatrixM.eye(5)).sum
    }

    assertResult(Some(25.0), "Q matrix is not orthogonal(test 2)") {
      val itest = res.Q.transpose |* res.Q
      val norm  = itest.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(5,5,norm) :== MatrixM.eye(5)).sum
    }

    assertResult(Some(25.0), "Q * R should yield original matrix") {
      val a1 = (res.Q |* res.R) :- mat
      val a2 = a1.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(5,5,a2) :== MatrixM.zero(5,5)).sum
    }

  }

  it should "match the values of the qr decompostion of a 5x5 matrix" in {

    val arrQ= Array(
      -0.2250175802	,
      -0.1125087901	,
      -0.5625439505	,
      -0.7875615306	,
      0	,
      0.0130470858	,
      -0.5088363471	,
      0.0326177146	,
      0.0456648004	,
      -0.85893315	,
      0.9397991177	,
      -0.1777908515	,
      0.0364886645	,
      -0.2691786723	,
      0.1066745109	,
      -0.2404755802	,
      -0.75265008	,
      0.3997580631	,
      -0.109312725	,
      0.451590048	,

      -0.0902550602	,
      0.3610202407	,
      0.7220404814	,
      -0.5415303611	,
      -0.2166121444
    )
    val Q = MatrixM(5,5,arrQ)
    val arrR = Array(
      -8.8881944173	,
      0	,
      0	,
      0	,
      0	,
      -0.6750527406	,
      -11.6423495823	,
      0	,
      0	,
      0	,
      -12.0384405399	,
      0.6980190915	,
      5.6203842142	,
      0	,
      0	,
      -7.6505977263	,
      -0.071758972	,
      4.2610651509	,
      -1.1430349339	,
      0	,
      -0.7875615306	,
      -9.5743864795	,
      -0.4978143841	,
      -2.1074202241	,
      1.010856674

    )
    val R = MatrixM(5,5,arrR)
    val arr = Array(2.0,1.0,5.0,7.0,0.0, 0.0,6.0,0.0,0.0,10.0, 8.0,0.0,7.0,8.0,0.0,  6.0,1.0,4.0,5.0,0.0 ,0.0,7.0,0.0,0.0,7.0)
    val mat  = MatrixM(5,5,arr)
    val res  = mat.qr

    assertResult(Some(25.0), "R should match") {
      val a1 = res.R :- R
      val a2 = a1.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(5,5,a2) :== MatrixM.zero(5,5)).sum
    }

    assertResult(Some(25.0), "Q should match") {
      val a1 = res.Q :- Q
      val a2 = a1.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(5,5,a2) :== MatrixM.zero(5,5)).sum
    }

  }

  it should "take the qr decompostion of a 5x6 matrix: fundamental invariants" in {
    val arr = Array(2.0,-10.0,5.0,7.0,89.67, 0.0,6.0,60.0,0.0,10.0, 8.0,-90.89,7.0,8.0,0.0,  6.0,1.0,4.0,5.0,0.0 ,0.0,7.0,0.0,0.0,7.0, -89.0, 201, 0, 5.9,6.9)
    val mat  = MatrixM(5,6,arr)
    val res = mat.qr

    assertResult(Some(25.0), "Q matrix is not orthogonal") {
      val itest = res.Q |* res.Q.transpose
      val norm  = itest.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(5,5,norm) :== MatrixM.eye(5)).sum
    }

    assertResult(Some(25.0), "Q matrix is not orthogonal(test 2)") {
      val itest = res.Q.transpose |* res.Q
      val norm  = itest.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(5,5,norm) :== MatrixM.eye(5)).sum
    }

    assertResult(Some(25.0), "Q * R should yield original matrix") {
      val a1 = (res.Q |* res.R) :- mat
      val a2 = a1.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(5,5,a2) :== MatrixM.zero(5,5)).sum
    }

  }
  it should "match the values of the qr decompostion of a 5x6 matrix" in {
    val arrR = Array(
      -90.6570951443	,
      0	,
      0	,
      0	,
      0	,
      -12.5384560159	,
      -59.8229648274	,
      0	,
      0	,
      0	,
      -11.2059624057	,
      4.4438698	,
      -91.0641012022	,
      0	,
      0	,
      -0.6287428459	,
      -3.9803533003	,
      -0.3926080932	,
      -7.8490336299	,
      0	,
      -6.1516420652	,
      -0.5828515293	,
      7.7151692504	,
      -0.4893955258	,
      0.2328936532	,
      16.8544667968	,
      -24.8454585107	,
      204.6295878394	,
      39.6809798191	,
      -63.6804660919
    )
    val arrQ = Array (
      -0.0220611525	,
      0.1103057624	,
      -0.0551528812	,
      -0.0772140337	,
      -0.9891117718	,
      0.0046238563	,
      -0.1234152131	,
      -0.9913996773	,
      0.0161834969	,
      0.0401507089	,
      -0.0849098123	,
      0.9784918037	,
      -0.118461719	,
      -0.0775588294	,
      0.1236752321	,
      -0.7607557556	,
      -0.122598737	,
      0.0034790253	,
      -0.6351632921	,
      0.0526850904	,

      0.6430652607	,
      -0.0111652944	,
      -0.0062894399	,
      -0.764413702	,
      0.0444358163
    )
    val Q = MatrixM(5,5,arrQ)
    val R = MatrixM(5,6,arrR)
    val arr = Array(2.0,-10.0,5.0,7.0,89.67, 0.0,6.0,60.0,0.0,10.0, 8.0,-90.89,7.0,8.0,0.0,  6.0,1.0,4.0,5.0,0.0 ,0.0,7.0,0.0,0.0,7.0, -89.0, 201, 0, 5.9,6.9)
    val mat  = MatrixM(5,6,arr)
    val res = mat.qr

    assertResult(Some(30.0), "R 5x6 should match") {
      val a1 = res.R :- R
      val a2 = a1.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(5,6,a2) :== MatrixM.zero(5,6)).sum
    }

    assertResult(Some(25.0), "Q 5x6 should match") {
      val a1 = res.Q :- Q
      val a2 = a1.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(5,5,a2) :== MatrixM.zero(5,5)).sum
    }

  }

  //
  // Section * LUDecompostionT
  //----------------------------------------
  //----------------------------------------
  //
  it should "match the values of the lu decompostion of a 6x6 matrix" in {

    val arrL = Array(
      1	,
      -0.1172031077	,
      -0.0554938957	,
      -0.0110987791	,
      -0.0776914539	,
      -0.0221975583	,
      0	,
      1	,
      -0.0329167272	,
      0.4255195466	,
      0.3598314199	,
      0.0653974713	,
      0	,
      0	,
      1	,
      -0.3012056227	,
      0.8918417053	,
      0.7288203713	,
      0	,
      0	,
      0	,
      1	,
      0.908812197	,
      0.3029234501	,
      0	,
      0	,
      0	,
      0	,
      1	,
      0.3275677427	,
      0	,
      0	,
      0	,
      0	,
      0	,
      1.0
    )
    val arrU = Array(
      -90.1	,
      0	,
      0	,
      0	,
      0	,
      0	,
      45	,
      15.2741398446	,
      0	,
      0	,
      0	,
      0	,
      100.9	,
      11.8257935627	,
      12.9886004941	,
      0	,
      0	,
      0	,
      34.56	,
      -40.9494605993	,
      4.5699468101	,
      20.1848633884	,
      0	,
      0	,
      0.89	,
      7.1043107658	,
      0.5832402267	,
      4.1625300533	,
      -6.230324877	,
      0	,
      -12	,
      3.5935627081	,
      22.4523615754	,
      -3.899538974	,
      71.2946219281	,
      -34.0376756807

    )
    val L = MatrixM(6,6,arrL)
    val U = MatrixM(6,6,arrU)
    val arr = Array(2.0,1.0,5.0,7.0,10.56,-90.1, 0.0,6.0,-3.0,2.0,10.0,45.0, 8.0,0.0,7.0,8.0,0.0,100.9,  6.0,1.0,4.0,5.0,-45.0, 34.56 ,0.09,7.0,0.3,0.56,7.0, 0.89,
      5.0, -9.0, 23.0, 90.0 , 5.0, -12.0)
    val mat  = MatrixM(6,6,arr)
    val res = mat.lu

    assertResult(Some(36.0), "P * L * U should match") {
      val P = res.constructP(res.permutations)
      val test = P |* res.L |* res.U
      val a1 = test :- mat
      val a2 = a1.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(6,6,a2) :== MatrixM.zero(6,6)).sum
    }

    assertResult(Some(36.0), "L should match") {
      val a1 = res.L :- L
      val a2 = a1.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(6,6,a2) :== MatrixM.zero(6,6)).sum
    }

    assertResult(Some(36.0), "U should match") {
      val a1 = res.U :- U
      val a2 = a1.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(6,6,a2) :== MatrixM.zero(6,6)).sum
    }


  }
  //
  // Section * CholeskyDecompostionT
  //----------------------------------------
  //----------------------------------------
  //
  it should "match the values of the cholesky decompostion of a 6x6 square matrix" in {
    val arrL = Array(
      11.3581732686	,
      -3.2020994169	,
      18.0510540869	,
      49.1320555517	,
      -19.6554494037	,
      68.1835081826	,
      0	,
      12.5597197152	,
      -12.4285202038	,
      -49.7426923319	,
      -2.6575993597	,
      43.5535492984	,
      0	,
      0	,
      12.1565345348	,
      55.118878598	,
      23.1703823278	,
      -58.0898301742	,
      0	,
      0	,
      0	,
      17.7743775961	,
      -6.7945561096	,
      77.5340560127	,
      0	,
      0	,
      0	,
      0	,
      36.5250799375	,
      33.354408821	,
      0	,
      0	,
      0	,
      0	,
      0	,
      67.9604918771

    )
    val L = MatrixM(6,6,arrL)
    val arr = Array(2.0,1.0,5.0,7.0,10.56,-90.1, 0.0,6.0,-3.0,2.0,10.0,45.0, 8.0,0.0,7.0,8.0,0.0,100.9,  6.0,1.0,4.0,5.0,-45.0, 34.56 ,0.09,7.0,0.3,0.56,7.0, 0.89,
      5.0, -9.0, 23.0, 90.0 , 5.0, -12.0)
    val matx  = MatrixM(6,6,arr)
    //println(matx)
    val mat   = matx |* matx.transpose

    val res = mat.cholesky

    assertResult(Some(36.0), "L hould match") {
      val a1 = res.L :- L
      val a2 = a1.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(6,6,a2) :== MatrixM.zero(6,6)).sum
    }

    assertResult(Some(36.0), "L * L^t should match") {
      val test = res.L |* res.L.transpose
      val a1 = test :- mat
      val a2 = a1.toArray.map(_.map(scala.math.abs).map((x) => scala.math.floor(x + 0.5)))
      (MatrixM(6,6,a2) :== MatrixM.zero(6,6)).sum
    }

  }

}
