[TOC]

------



#Introduction

MatrixMeta is a Scala API around a set of matrix libraries. 
It uses Scala implicits to enable the user to switch between different implementations. 

The goal is to provide a uniform api for both dense and sparse matrices as well as different packages

#Building

```shell
sbt compile
```

#Testing

```shell
sbt test
```

#Synopsis

## Example

```scala
         

import com.kabouterlabs.matrix.MatrixOperations._

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
    val m4 = evmatrix.one(10, 20)
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
```
## Output
```
INFO: successfully loaded /var/folders/_8/yv63b6sn4wndy7sxxkvqs7880000gn/T/jniloader2467803679675101495netlib-native_system-osx-x86_64.jnilib
{breeze.linalg.DenseMatrix
5.0  8.0  7.0  
6.0  9.0  6.0  
7.0  8.0  5.0  }
{breeze.linalg.DenseMatrix
78.23  78.23  78.23  
78.23  78.23  78.23  
78.23  78.23  78.23  }
{breeze.linalg.DenseMatrix$mcD$sp
122398.65800000002  122398.65800000002  122398.65800000002  
128518.59090000001  128518.59090000001  128518.59090000001  
122398.65800000002  122398.65800000002  122398.65800000001  }
{breeze.linalg.DenseMatrix$mcD$sp
0.0  0.0  0.0  0.0  0.0  
0.0  0.0  0.0  0.0  0.0  
0.0  0.0  0.0  0.0  0.0  
0.0  0.0  0.0  0.0  0.0  
0.0  0.0  0.0  0.0  0.0  }
{breeze.linalg.DenseMatrix
4.0   6.0  2.0  4.0  6.0    -89.0   
90.0  7.0  3.0  5.0  -34.0  -12.78  }
{breeze.linalg.DenseMatrix
-88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  ... (20 total)
-88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  ...
-88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  ...
-88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  ...
-88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  ...
-88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  ...
-88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  ...
-88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  ...
-88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  ...
-88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  -88.89  ...}
{breeze.linalg.DenseMatrix$mcD$sp
1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  ... (20 total)
1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  ...
1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  ...
1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  ...
1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  ...
1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  ...
1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  ...
1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  ...
1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  ...
1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  ...}
{breeze.linalg.DenseMatrix
-177.78  -177.78  -177.78  -177.78  -177.78  -177.78  -177.78  ... (20 total)
-177.78  -177.78  -177.78  -177.78  -177.78  -177.78  -177.78  ...
-177.78  -177.78  -177.78  -177.78  -177.78  -177.78  -177.78  ...
-177.78  -177.78  -177.78  -177.78  -177.78  -177.78  -177.78  ...
-177.78  -177.78  -177.78  -177.78  -177.78  -177.78  -177.78  ...
-177.78  -177.78  -177.78  -177.78  -177.78  -177.78  -177.78  ...
-177.78  -177.78  -177.78  -177.78  -177.78  -177.78  -177.78  ...
-177.78  -177.78  -177.78  -177.78  -177.78  -177.78  -177.78  ...
-177.78  -177.78  -177.78  -177.78  -177.78  -177.78  -177.78  ...
-177.78  -177.78  -177.78  -177.78  -177.78  -177.78  -177.78  ...}
{breeze.linalg.DenseMatrix
-1777.8  -1777.8  -1777.8  -1777.8  -1777.8  -1777.8  -1777.8  ... (20 total)}
{breeze.linalg.DenseMatrix
-3555.6000000000017  
-3555.6000000000017  
-3555.6000000000017  
-3555.6000000000017  
-3555.6000000000017  
-3555.6000000000017  
-3555.6000000000017  
-3555.6000000000017  
-3555.6000000000017  
-3555.6000000000017  }
Dec 04, 2016 8:53:24 PM com.github.fommil.jni.JniLoader load
INFO: already loaded netlib-native_system-osx-x86_64.jnilib
({breeze.linalg.DenseMatrix
81.35673399422318   0.0  
64.27353909626761   0.0  
7.288047100648987   0.0  
-61.91832019113972  0.0  },{breeze.linalg.DenseMatrix$mcD$sp
-0.6629665943954017  0.7792374389558067    -0.9950527650125266  -0.440534711331003    
0.19977532633158002  -0.06170138283576464  0.07044812813403961  -0.6020036496147592   
0.3342156301456594   -0.22191347958969848  0.05547771299741258  0.454776812720665     
0.6394255439620891   -0.5828690767763739   0.04277007650112591  -0.48651703420810816  })
{breeze.linalg.DenseMatrix
434.0   501.0  959.0    
417.0   527.0  1434.0   
-489.0  139.0  -1668.0  }
{breeze.linalg.DenseMatrix
4.0  7.0   56.0  
5.0  8.0   -1.0  
6.0  21.0  -9.0  }
{breeze.linalg.DenseMatrix
434.0   501.0  959.0    4.0  7.0   56.0  
417.0   527.0  1434.0   5.0  8.0   -1.0  
-489.0  139.0  -1668.0  6.0  21.0  -9.0  }
{breeze.linalg.DenseMatrix
434.0   501.0  959.0    
417.0   527.0  1434.0   
-489.0  139.0  -1668.0  
4.0     7.0    56.0     
5.0     8.0    -1.0     
6.0     21.0   -9.0     }
{breeze.linalg.DenseMatrix
434.0   501.0  959.0    
417.0   527.0  1434.0   
-489.0  139.0  -1668.0  }
{breeze.linalg.DenseMatrix
4.0  7.0   56.0  
5.0  8.0   -1.0  
6.0  21.0  -9.0  }
com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit$MatrixOperationsTC$implicit$$@57bc27f5
{jeigen.DenseMatrix
DenseMatrix, 3 * 3:

 5.00000  8.00000  7.00000 
 6.00000  9.00000  6.00000 
 7.00000  8.00000  5.00000 

}
{jeigen.DenseMatrix
DenseMatrix, 3 * 3:

78.23000 78.23000 78.23000 
78.23000 78.23000 78.23000 
78.23000 78.23000 78.23000 

}
{jeigen.DenseMatrix
DenseMatrix, 3 * 3:

122398.65800 122398.65800 122398.65800 
128518.59090 128518.59090 128518.59090 
122398.65800 122398.65800 122398.65800 

}
{jeigen.DenseMatrix
DenseMatrix, 5 * 5:

 0.00000  0.00000  0.00000  0.00000  0.00000 
 0.00000  0.00000  0.00000  0.00000  0.00000 
 0.00000  0.00000  0.00000  0.00000  0.00000 
 0.00000  0.00000  0.00000  0.00000  0.00000 
 0.00000  0.00000  0.00000  0.00000  0.00000 

}
{jeigen.DenseMatrix
DenseMatrix, 2 * 6:

 4.00000  6.00000  2.00000  4.00000  6.00000 -89.00000 
90.00000  7.00000  3.00000  5.00000 -34.00000 -12.78000 

}
{jeigen.DenseMatrix
DenseMatrix, 10 * 20:

-88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 
-88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 
-88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 
-88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 
-88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 
-88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 
-88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 
-88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 
-88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 
-88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 -88.89000 

}
{jeigen.DenseMatrix
DenseMatrix, 10 * 20:

 1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000 
 1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000 
 1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000 
 1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000 
 1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000 
 1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000 
 1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000 
 1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000 
 1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000 
 1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000 

}
{jeigen.DenseMatrix
DenseMatrix, 10 * 20:

-177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 
-177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 
-177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 
-177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 
-177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 
-177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 
-177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 
-177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 
-177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 
-177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 -177.78000 

}
{jeigen.DenseMatrix
DenseMatrix, 1 * 20:

-1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 -1777.80000 

}
{jeigen.DenseMatrix
DenseMatrix, 10 * 1:

-3555.60000 
-3555.60000 
-3555.60000 
-3555.60000 
-3555.60000 
-3555.60000 
-3555.60000 
-3555.60000 
-3555.60000 
-3555.60000 

}
({jeigen.DenseMatrix
DenseMatrix, 4 * 2:

 7.28805  0.00000 
81.35673  0.00000 
64.27354  0.00000 
-61.91832  0.00000 

},{jeigen.DenseMatrix
DenseMatrix, 4 * 8:

-0.99505 -0.66297  0.77924 -0.44053  0.00000  0.00000  0.00000  0.00000 
 0.07045  0.19978 -0.06170 -0.60200  0.00000  0.00000  0.00000  0.00000 
 0.05548  0.33422 -0.22191  0.45478  0.00000  0.00000  0.00000  0.00000 
 0.04277  0.63943 -0.58287 -0.48652  0.00000  0.00000  0.00000  0.00000 

})
{jeigen.DenseMatrix
DenseMatrix, 3 * 3:

434.00000 501.00000 959.00000 
417.00000 527.00000 1434.00000 
-489.00000 139.00000 -1668.00000 

}
{jeigen.DenseMatrix
DenseMatrix, 3 * 3:

 4.00000  7.00000 56.00000 
 5.00000  8.00000 -1.00000 
 6.00000 21.00000 -9.00000 

}
{jeigen.DenseMatrix
DenseMatrix, 3 * 6:

434.00000 501.00000 959.00000  4.00000  7.00000 56.00000 
417.00000 527.00000 1434.00000  5.00000  8.00000 -1.00000 
-489.00000 139.00000 -1668.00000  6.00000 21.00000 -9.00000 

}
{jeigen.DenseMatrix
DenseMatrix, 6 * 3:

434.00000 501.00000 959.00000 
417.00000 527.00000 1434.00000 
-489.00000 139.00000 -1668.00000 
 4.00000  7.00000 56.00000 
 5.00000  8.00000 -1.00000 
 6.00000 21.00000 -9.00000 

}
{jeigen.DenseMatrix
DenseMatrix, 3 * 3:

434.00000 501.00000 959.00000 
417.00000 527.00000 1434.00000 
-489.00000 139.00000 -1668.00000 

}
{jeigen.DenseMatrix
DenseMatrix, 3 * 3:

 4.00000  7.00000 56.00000 
 5.00000  8.00000 -1.00000 
 6.00000 21.00000 -9.00000 

}
com.kabouterlabs.matrix.implicits.jeigen.JeigenDenseMatrixImplicit$MatrixOperationsTC$implicit$$@6d5620ce
{org.armadillojava.Mat
(3, 3)-matrix: [
     5.0000    8.0000    7.0000
     6.0000    9.0000    6.0000
     7.0000    8.0000    5.0000
]}
{org.armadillojava.Mat
(3, 3)-matrix: [
    78.2300   78.2300   78.2300
    78.2300   78.2300   78.2300
    78.2300   78.2300   78.2300
]}
{org.armadillojava.Mat
(3, 3)-matrix: [
  122398.6580 122398.6580 122398.6580
  128518.5909 128518.5909 128518.5909
  122398.6580 122398.6580 122398.6580
]}
{org.armadillojava.Mat
(5, 5)-matrix: [
          0         0         0         0         0
          0         0         0         0         0
          0         0         0         0         0
          0         0         0         0         0
          0         0         0         0         0
]}
{org.armadillojava.Mat
(2, 6)-matrix: [
     4.0000    6.0000    2.0000    4.0000    6.0000  -89.0000
    90.0000    7.0000    3.0000    5.0000  -34.0000  -12.7800
]}
{org.armadillojava.Mat
(10, 20)-matrix: [
   -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900
   -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900
   -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900
   -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900
   -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900
   -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900
   -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900
   -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900
   -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900
   -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900  -88.8900
]}
{org.armadillojava.Mat
(10, 20)-matrix: [
     1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000
     1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000
     1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000
     1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000
     1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000
     1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000
     1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000
     1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000
     1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000
     1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000
]}
{org.armadillojava.Mat
(10, 20)-matrix: [
  -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800
  -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800
  -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800
  -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800
  -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800
  -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800
  -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800
  -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800
  -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800
  -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800 -177.7800
]}
{org.armadillojava.Mat
(1, 20)-matrix: [
 -1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000-1777.8000
]}
{org.armadillojava.Mat
(10, 1)-matrix: [
 -3555.6000
 -3555.6000
 -3555.6000
 -3555.6000
 -3555.6000
 -3555.6000
 -3555.6000
 -3555.6000
 -3555.6000
 -3555.6000
]}
({scala.None$
None},{scala.None$
None})
{org.armadillojava.Mat
(3, 3)-matrix: [
   434.0000  501.0000  959.0000
   417.0000  527.0000 1434.0000
  -489.0000  139.0000-1668.0000
]}
{org.armadillojava.Mat
(3, 3)-matrix: [
     4.0000    7.0000   56.0000
     5.0000    8.0000   -1.0000
     6.0000   21.0000   -9.0000
]}
{org.armadillojava.Mat
(3, 6)-matrix: [
   434.0000  501.0000  959.0000    4.0000    7.0000   56.0000
   417.0000  527.0000 1434.0000    5.0000    8.0000   -1.0000
  -489.0000  139.0000-1668.0000    6.0000   21.0000   -9.0000
]}
{org.armadillojava.Mat
(6, 3)-matrix: [
   434.0000  501.0000  959.0000
   417.0000  527.0000 1434.0000
  -489.0000  139.0000-1668.0000
     4.0000    7.0000   56.0000
     5.0000    8.0000   -1.0000
     6.0000   21.0000   -9.0000
]}
{org.armadillojava.Mat
(3, 3)-matrix: [
   434.0000  501.0000  959.0000
   417.0000  527.0000 1434.0000
  -489.0000  139.0000-1668.0000
]}
{org.armadillojava.Mat
(3, 3)-matrix: [
     4.0000    7.0000   56.0000
     5.0000    8.0000   -1.0000
     6.0000   21.0000   -9.0000
]}
com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaMatImplicit$MatrixOperationsTC$implicit$$@6bb4dd34
DONE

Process finished with exit code 0

```

  






# Matrix Libraries Covered

  1. [breeze](https://github.com/scalanlp/breeze)

```scala

import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
```

  2. [jeigen](https://github.com/hughperkins/jeigen)


```scala

import com.kabouterlabs.matrix.implicits.jeigen.JeigenDenseMatrixImplicit._

```

  3. [armadillojava](https://github.com/SRAhub/ArmadilloJava)

```scala

    import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaMatImplicit._
```





#API

## Creation



|               | Direct         |   Implicit |
|---------------|----------------|-------------------
| Empty Matrix  |   MatrixM(row,coll) | matrix(row,coll)
| Initialize matrix with Array |  MatrixM(row,coll,data) | matrix(row,coll,data)
| Matrix initialized with 0 | MatrixM.zero(row,coll) | zero(row,coll)
| Matrix initialized with 1 | MatrixM.ones(row,coll) | ones(row,coll)
| Identity Matrix of size d | MatrixM.eye(dim) | eye(dim)
| Matrix with random values | MatrixM.rand(row,coll) | rand(row,coll)
| Diagonal matrix initialized with data array D | MatrixM.diag(D) | diag(D)
| Fill matrix with value V | MatrixM.fill(row,coll, V) | fill(row,coll,V) 
| No matrix created  | MatrixM.none | none 



 





```

```