package test.kabouterlabs.nr.matrix.implicits

import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
import com.kabouterlabs.matrix.MatrixM
import com.kabouterlabs.matrix.MatrixOperations._
import org.scalatest._


/**
  * Created by fons on 4/19/16.
  */
trait MatrixImplicit$TestT[U] extends Matchers {
  this: FlatSpec =>


  val t = Array[Double]( 4.0, 5.0, 6.0, 7.0, 8.0,21.0,
    56.0,-1.0,-9.0,67.0,45.0,89.0,
    23.0,67.0,-78.0,23.0,45.0,-65.0,
    90.0,89.0)

  val rows  = 6
  val colls = 3
    def throwAssert () {
      assert(1==2, "ok get a clue")
    }

  "a matrix" should "be created" in {
    MatrixM(rows,colls,t)  should not equal None
  }

//  it should "the infix operator for element wise add" in {
//    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
//    val l2 = MatrixM(rows,colls,t)
//    val l0 = add(l3,l2)(matrixOps)
//
//  }
//
//  it should "the infix operator for element wise multiply" in {
//    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
//    val l2 = MatrixM(rows,colls,t)
//    val l0 = multe(l3,l2)
//
//  }
//
//  it should "the infix operator for matrix multiply" in {
//    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
//    val l2 = MatrixM(rows,colls,t)
//    val l0 = mult(l3,l2)
//
//  }

  it should "the infix operator for element wise add" in {
    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
    val l2 = MatrixM(rows,colls,t)
    val l0 = add(l3,l2)

  }

  it should "the infix operator for element wise subtract" in {
    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
    val l2 = MatrixM(rows,colls,t)
    val l0 = sub(l3,l2)
  }

  it should "use the infix operator for element wise divide" in {
    val l3 = MatrixM(3,3,Array(4.0,5.0,6.0,7.0,8.0,21.0,56.0,-1.0,-9.0))
    val l2 = MatrixM(rows,colls,t)
    val l0 = div(l3,l2)

  }

}
