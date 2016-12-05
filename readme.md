
#Introduction

MatrixMeta is a Scala API around a set of matrix libraries. 
It uses Scala implicits to enable the user to switch between different implementations. 

The goal is to provide a uniform api for both dense and sparse matrices as well as different packages

#Building

    sbt compile

#Testing

    sbt test
    
#Synopsis

```scala
         

import com.kabouterlabs.matrix.MatrixOperations._

/**
  * Created by fons on 12/2/16.
  */

case class MatrixExample[T](implicit evmatrix: MatrixOperationsTC[T]) {

  def apply(): Unit = {
    val m1a = matrix(3, 3, Array(5, 6, 7, 8, 9, 8, 7, 6, 5))
    val m2a = fill(3, 3, 78.23)
    val m3a = m1a |* m2a :* m2a
    println(m1a)
    println(m2a)
    println(m3a)



    val ar = Array(4.0, 90.0, 6.0, 7.0, 2.0, 3.0, 4.0, 5.0, 6.0, -34.0, -89.0, -12.78)

    val m1 = zero(5, 5)
    val m2 = matrix(2, 6, ar)

    println(m1)
    println(m2)
    val m3 = evmatrix.fill(10, 20, -88.89)
    println(m3)
    val m4 = evmatrix.ones(10, 20)
    println(m4)
    val m6 = m3 :+ m3

    println(m6)
    val m7 = add(m3, m3)
    val m7a = m6.sumRows()
    println(m7a)
    val m8 = m6.sumCols()
    println(m8)

    val a1 = Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0, 90.0, 33.0, 107.0, -78.0, -23.0, 14.0, 33.0)
    val er1 = Array(81.3567339942231, 64.2735390962677, 7.28804710064899, -61.9183201911397)
    val hsize = math.sqrt(a1.length).toInt
    val l3 = matrix(hsize, hsize, a1)
    val ey = eigen(l3)
    println(ey)




    val r = Array[Double](434.00000, 417.00000, -489.00000, 501.00000, 527.00000, 139.00000,
      959.00000, 1434.00000, -1668.00000, 1068.00000, 1361.00000, -506.00000,
      -39.00000,
      -322.00000,
      1047.00000,
      118.00000,
      -2.00000,
      1672.00000)


    val l2 = matrix(3, 3, r)
    val l3a = matrix(3, 3, Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0))
    println(l2)
    println(l3a)
    val l4 = l2 concatRight l3a
    println(l4)
    val l4i = l2.inverse()
    inverse(l2) :== l4i
    val l5 = l2 concatDown l3a
    println(l5)
    val cv = l4.slice(::, 0 to 2)
    println(cv)
    val a = (l4.slice(::, 0 to 2) :== l2).sum()
    val b = (l4.slice(::, 3 to 5) :== l3a).sum()
    val l6a = l4.slice(::, 3 to 5)
    println(l6a)
    println(evmatrix)

  }

}
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
      import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaMatImplicit._
      new MatrixExample
    }

}

object Main extends App {
  Use.run1()()
  Use.run2()()
  val t = Use.run3()
  t()
  println("DONE")
}

````

</code>


#API

## Matrix Libraries


## Matrix Creation

## 