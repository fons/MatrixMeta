


Table of Contents
=================
[![Build Status](https://travis-ci.org/fons/MatrixMeta.svg?branch=master)](https://travis-ci.org/fons/MatrixMeta)


---
* [MatrixMeta](#matrixmeta)
     * [Introduction](#introduction)
     * [Building](#building)
     * [Testing](#testing)
     * [Matrix Libraries Covered](#matrix-libraries-covered)
* [API Reference](#api-reference)
     * [Usage](#usage)
        * [Directly](#directly)
        * [As implicit parameter](#as-implicit-parameter)
     * [Matrix Creation](#matrix-creation)
        * [Synopsis](#synopsis)
        * [Operations](#operations)
     * [Indexing ,Slicing, Aggregation](#indexing-slicing-aggregation)
        * [Synopsis](#synopsis-1)
        * [Operations](#operations-1)
     * [Matrix Algebraic Operations](#matrix-algebraic-operations)
        * [Synopsis](#synopsis-2)
        * [Operations](#operations-2)
     * [Linear Algebra](#linear-algebra)
        * [Synopsis](#synopsis-3)
        * [Operations](#operations-3)
        * [Eigen Values and Eigen Vectors](#eigen-values-and-eigen-vectors)
     * [Serialization](#serialization)
        * [Synopsis](#synopsis-4)
        * [Operations](#operations-4)

Created by [gh-md-toc](https://github.com/ekalinin/github-markdown-toc)



---

# MatrixMeta

##Introduction

MatrixMeta enables you to use different matrix library implementations seamlessly.



##Building

```shell
sbt compile
```

##Testing

```shell
sbt test
```

##Matrix Libraries Covered

* [breeze](https://github.com/scalanlp/breeze)

```scala
import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
```

* [jeigen](https://github.com/hughperkins/jeigen)

```scala
import com.kabouterlabs.matrix.implicits.jeigen.JeigenDenseMatrixImplicit._
```

* [armadillojava](https://github.com/SRAhub/ArmadilloJava)

```scala
import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaDenseMatrixImplicit._
```


* [apache commons math](http://commons.apache.org/proper/commons-math/userguide/linear.html)

```scala
import com.kabouterlabs.matrix.implicits.apachecommonsmath.ApacheCommonsMathDenseMatrixImplicit._
```

* [jblas](http://jblas.org/)

```scala
   import com.kabouterlabs.matrix.implicits.jblass.JblasDoubleMatrixImplicit._
```

   

Note that both breeze and armadillojava use [netlib-java](https://github.com/fommil/netlib-java). jeigen uses [jna](https://github.com/java-native-access/jna) to wrap the [eigen](http://eigen.tuxfamily.org/index.php?title=Main_Page) C++ library.

------



#API Reference

## Usage

### Directly

```scala
import com.kabouterlabs.matrix.MatrixM
import com.kabouterlabs.matrix.MatrixOperations._
import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
 //or one of the other implicits 
 
```



### As implicit parameter

```scala
import com.kabouterlabs.matrix.MatrixOperations._

case class MatrixExample[T](implicit evmatrix: MatrixOperationsTC[T]) {
  [...]
}

//then 
import com.kabouterlabs.matrix.implicits.breeze.BreezeDenseMatrixImplicit._
new MatrixExample
// will use breeze as the underlying matrix library
```

## Matrix Creation

### Synopsis

```scala
import com.kabouterlabs.matrix.MatrixM
import com.kabouterlabs.matrix.MatrixOperations._
import com.kabouterlabs.matrix.implicits.armadillojava.ArmadilloJavaDenseMatrixImplicit._
  val mat1 = MatrixM(2,2)
  val mat2 = MatrixM.one(2,2)
  val mat3 = MatrixM.rand(2,4)

  val r000 = Array[Double](434.00000, 417.00000,  -489.00000,  501.00000,   527.00000,   139.00000,
    959.00000,  1434.00000,  -1668.00000,   1068.00000,   1361.00000,   -506.00000,
    -39.00000, -322.00000, 1047.00000, 118.00000, -2.00000, 1672.00000)
  val mat4 = MatrixM(3, 3, r000)
  println(mat1,mat2,mat3,mat4)
```



```
({org.armadillojava.Mat
(2, 2)-matrix: [
          0         0
          0         0
]},{org.armadillojava.Mat
(2, 2)-matrix: [
     1.0000    1.0000
     1.0000    1.0000
]},{org.armadillojava.Mat
(2, 4)-matrix: [
     0.5774    0.1570    0.4965    0.4801
     0.4786    0.7773    0.9585    0.3017
]},{org.armadillojava.Mat
(3, 3)-matrix: [
   434.0000  501.0000  959.0000
   417.0000  527.0000 1434.0000
  -489.0000  139.0000-1668.0000
]})

```

### Operations

| Operation                                | Direct                    | Implicit scope        |
| :--------------------------------------- | ------------------------- | --------------------- |
| Empty Matrix                             | MatrixM(row,coll)         | matrix(row,coll)      |
| Initialize matrix with Array             | MatrixM(row,coll,data)    | matrix(row,coll,data) |
| Matrix initialized with 0                | MatrixM.zero(row,coll)    | zero(row,coll)        |
| Matrix initialized with 1                | MatrixM.ones(row,coll)    | ones(row,coll)        |
| Identity Matrix of size d                | MatrixM.eye(dim)          | eye(dim)              |
| Matrix with random values                | MatrixM.rand(row,coll)    | rand(row,coll)        |
| Diagonal matrix initialized with data array D | MatrixM.diag(D)           | diag(D)               |
| Fill matrix with value V                 | MatrixM.fill(row,coll, V) | fill(row,coll,V)      |
| No matrix created                        | MatrixM.none              | none                  |



## Indexing ,Slicing, Aggregation

### Synopsis

```scala
val mm = MatrixM.rand(10,10)
val ss = mm(::,0 to 3)
println(ss)
val mm = MatrixM.rand(10,10)
val mc = mm.deepCopy(2,1,89.90)

val s1 = mm(2,1)
val s2 = mc(2,1)

println(s1,s2, mm, mm(::,2))
val mmd = MatrixM.rand(10,10)
val ss = mmd(::,0 to 3)
ss(2,1,909.890)
println(mmd, ss)
```



```reStructuredText
{breeze.linalg.DenseMatrix
0.2837778444775749   0.8579650113588222   0.3418471653423856   0.9307517078612046  
0.05949754250565076  0.03158873862870193  0.863421443055354    0.5658424105106379  
0.6173752477462089   0.5905474153790615   0.4529669744171494   0.2813252987181811  
0.10652573135517485  0.34370509210435674  0.43048537165122536  0.6268659904851073  
0.45979000662576897  0.18373298972108065  0.5593474321779575   0.8359163014022442  
0.8310672643442611   0.38242429289741     0.753462759853414    0.3076126032691908  
0.482921417268668    0.3111265557321392   0.8801397061735807   0.7895732193230647  
0.6331731597239412   0.19465081351603875  0.6839996610857064   0.641202992419446   
0.27553195658567886  0.8455495127829      0.9752823594167976   0.7836903228091154  
0.6306358214285563   0.9203409695192428   0.17947790404678599  0.172010385465164   }

(Some(0.42567356858385597),Some(89.9),{org.armadillojava.Mat
(10, 10)-matrix: [
     0.9443    0.3637    0.3873    0.5389    0.8672    0.9705    0.5294    0.0068    0.7316    0.8691
     0.6178    0.2015    0.8349    0.4145    0.4907    0.4075    0.1076    0.7748    0.3073    0.4547
     0.2799    0.4257    0.3322    0.6440    0.5811    0.0056    0.9458    0.6157    0.6868    0.5537
     0.3688    0.7542    0.7775    0.4294    0.0779    0.0603    0.3285    0.3470    0.1619    0.2664
     0.1439    0.3660    0.6493    0.2465    0.0338    0.9230    0.0900    0.6601    0.9080    0.4308
     0.3971    0.7697    0.7752    0.5495    0.8491    0.0195    0.3494    0.4458    0.4480    0.3629
     0.1676    0.4755    0.7071    0.9824    0.1014    0.1042    0.8003    0.0919    0.6893    0.7565
     0.1933    0.3919    0.3064    0.0598    0.2035    0.7171    0.0365    0.7063    0.2117    0.6879
     0.8278    0.4397    0.5193    0.9183    0.9716    0.0650    0.9458    0.0417    0.2354    0.7839
     0.0329    0.0367    0.9378    0.9337    0.0051    0.4911    0.5642    0.2295    0.5699    0.9362
]},{org.armadillojava.Mat
(10, 1)-matrix: [
     0.3873
     0.8349
     0.3322
     0.7775
     0.6493
     0.7752
     0.7071
     0.3064
     0.5193
     0.9378
]})
({org.armadillojava.Mat
(10, 10)-matrix: [
     0.9767    0.1935    0.2006    0.0691    0.9723    0.5412    0.7768    0.8709    0.8311    0.3186
     0.9627    0.0970    0.7743    0.2217    0.5707    0.2462    0.0403    0.6931    0.9136    0.5338
     0.2679    0.7135    0.0595    0.9683    0.9712    0.6655    0.3541    0.6716    0.9339    0.0543
     0.3842    0.8059    0.4064    0.8324    0.6826    0.3697    0.0069    0.3120    0.5967    0.2551
     0.6069    0.9869    0.9313    0.7605    0.1793    0.6175    0.0774    0.8391    0.0227    0.2118
     0.7545    0.3209    0.3934    0.1288    0.6279    0.6041    0.6899    0.3586    0.5652    0.0706
     0.6238    0.2390    0.8898    0.6155    0.9154    0.9755    0.8470    0.3526    0.0510    0.2881
     0.3353    0.2764    0.2377    0.2725    0.8782    0.4053    0.8807    0.0290    0.8166    0.6566
     0.1242    0.9587    0.4371    0.1380    0.4685    0.6664    0.7524    0.0861    0.9752    0.5224
     0.3591    0.5362    0.7141    0.6877    0.0248    0.7034    0.3129    0.1988    0.7590    0.0730
]},{org.armadillojava.Mat
(10, 4)-matrix: [
     0.9767    0.1935    0.2006    0.0691
     0.9627    0.0970    0.7743    0.2217
     0.2679  909.8900    0.0595    0.9683
     0.3842    0.8059    0.4064    0.8324
     0.6069    0.9869    0.9313    0.7605
     0.7545    0.3209    0.3934    0.1288
     0.6238    0.2390    0.8898    0.6155
     0.3353    0.2764    0.2377    0.2725
     0.1242    0.9587    0.4371    0.1380
     0.3591    0.5362    0.7141    0.6877
]})
```



### Operations

| Operation                                | Class Method               | Function                         |
| ---------------------------------------- | -------------------------- | -------------------------------- |
| deep copy ; Use this to avoid aliasing   | A.deepCopy                 | deepCopy(A)                      |
| get value  returns Option[Double]        | A(row,coll)                | getValue(A)                      |
| set value                                | A(row, col, value)         | setValue(A,row,col,value)        |
| return a sub matrix reference ;          | A( r_0 to r_1, c_0 to c_1) | slice(A, r_0 to r_1, c_0 to c_1) |
| extract a column; :: is a short cut for the whole range | A(::, 2)                   | slice(A, ::, 2)                  |
| concatenate a matrix to the bottom of the other | A concatDown B             | concatDown(A, B)                 |
| concatenate a matrix to the right of the other | A concatRight B            | concatRight(A,B)                 |
| create a matrix with just the diagonal values | A.toDiag                   | toDiag (A)                       |
| Sum all values across all rows           | A.sumRows                  | sumRows(A)                       |
| Sum all values acro                      |                            |                                  |



## Matrix Algebraic Operations

### Synopsis

```scala
  val l2 = rand(3, 3)
  val l3a = matrix(3, 3, Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0))
  val s1 = l2 :+ l3a
  val s2 = l2 :- l3a
  val s3 = l2 :\ l3a
  val s4 = l2 :* l3a
  val s5 = l2 |* l3a

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
  println(l2,l3a)
  println("------------")
  println(s1,s5,s1a,s3a,ta5)
```



```
({org.armadillojava.Mat
(3, 3)-matrix: [
     0.6720    0.8165    0.8766
     0.5588    0.6270    0.2185
     0.4778    0.8910    0.6804
]},{org.armadillojava.Mat
(3, 3)-matrix: [
     4.0000    7.0000   56.0000
     5.0000    8.0000   -1.0000
     6.0000   21.0000   -9.0000
]})
------------
({org.armadillojava.Mat
(3, 3)-matrix: [
     4.6720    7.8165   56.8766
     5.5588    8.6270   -0.7815
     6.4778   21.8910   -8.3196
]},{org.armadillojava.Mat
(3, 3)-matrix: [
    12.0299   29.6440   28.9285
     6.6812   13.5157   28.7022
    10.4482   24.7599   19.7425
]},{org.armadillojava.Mat
(3, 3)-matrix: [
     7.6720    7.8165    7.8766
     7.5588    7.6270    7.2185
     7.4778    7.8910    7.6804
]},{org.armadillojava.Mat
(3, 3)-matrix: [
     4.7043    5.7152    6.1360
     3.9119    4.3892    1.5292
     3.3446    6.2368    4.7625
]},{org.armadillojava.Mat
(3, 3)-matrix: [
          0    1.0000    1.0000
          0    1.0000         0
          0    1.0000         0
]})

```



### Operations



| Operation                                | Infix   | Function                  |
| ---------------------------------------- | ------- | ------------------------- |
| Elementwise addition                     | A :+B   | add(A,B)                  |
| Elementwise subtraction                  | A :- B  | subtract(A,B)             |
| Elementwise multiplication               | A :* B  | schur(A,B); hadamard(A,B) |
| Elementwise division                     | A :\ B  | divide(A,B)               |
| Matrix multiplication                    | A \|* B | multiply(A,B)             |
| Add value to every matrix element        | A ++ k  | add1(A,k)                 |
| Subtract value from every matrix element | A — k   | subtract1(A,k)            |
| Multiply every matrix element with a value | A ** k  | multply1(A,k)             |
| Divide every matrix element with a value | A \\\ k | divide1(A,k)              |



## Linear Algebra

### Synopsis

```scala
val mm0 = rand(3, 3)
val mm1= matrix(3, 3, Array(4.0, 5.0, 6.0, 7.0, 8.0, 21.0, 56.0, -1.0, -9.0))
println("mm0", mm0, "mm1", mm1)
println("mm1.inverse", mm1.inverse,"mm1.transpose", mm1.transpose, "mm1.determinant" ,mm1.determinant)
val res = mm1.solve(mm0)
println("solving mm1 * res = mm0; res = ", res, "residual : ",(mm1 |* res) :- mm0 )
val eigr = eigen(mm0)
println("(complex) eigen values : " , eigr._1, "eigen vectors :", eigr._2)
```

```reStructuredText
(mm0,{jeigen.DenseMatrix
DenseMatrix, 3 * 3:

 0.09195  0.17065  0.16146 
 0.96391  0.06410  0.20932 
 0.54790  0.74146  0.11060 

},mm1,{jeigen.DenseMatrix
DenseMatrix, 3 * 3:

 4.00000  7.00000 56.00000 
 5.00000  8.00000 -1.00000 
 6.00000 21.00000 -9.00000 

})
(mm1.inverse,{jeigen.DenseMatrix
DenseMatrix, 3 * 3:

-0.01564  0.37994 -0.13953 
 0.01196 -0.11408  0.08709 
 0.01748 -0.01288 -0.00092 

},mm1.transpose,{jeigen.DenseMatrix
DenseMatrix, 3 * 3:

 4.00000  5.00000  6.00000 
 7.00000  8.00000 21.00000 
56.00000 -1.00000 -9.00000 

},mm1.determinant,Some(3074.0095192193585))
(solving mm1 * res = mm0; res = ,{jeigen.DenseMatrix
DenseMatrix, 3 * 3:

 0.28835 -0.08177  0.06158 
-0.06114  0.05930 -0.01232 
-0.01131  0.00148  0.00002 

},residual : ,{jeigen.DenseMatrix
DenseMatrix, 3 * 3:

 0.00000  0.00000  0.00000 
 0.00000  0.00000 -0.00000 
-0.00000  0.00000 -0.00000 

})
((complex) eigen values : ,{jeigen.DenseMatrix
DenseMatrix, 3 * 2:

 0.85245  0.00000 
-0.29290  0.16902 
-0.29290 -0.16902 

},eigen vectors :,{jeigen.DenseMatrix
DenseMatrix, 3 * 6:

-0.29046  0.21151  0.21151  0.00000  0.02746 -0.02746 
-0.56098 -0.05410 -0.05410  0.00000 -0.50279  0.50279 
-0.77520 -0.47572 -0.47572  0.00000  0.68736 -0.68736 

})

```



### Operations

| Operation                                | Class method    | Function         |
| ---------------------------------------- | --------------- | ---------------- |
| Inverse                                  | A.inverse       | inverse(A)       |
| Transpose                                | A.transpose     | transpose(A)     |
| Determinant; Returns Option[Double]      | A.determinant   | determinant(A)   |
| Trace; Returns Option[Double]            | A.trace         | trace(A)         |
| Solve system A * R = B                   | R = A.solve(B)  | R = solve (A,B)  |
| Eigen results as special eigen result type | E = A.eig       | E = eig(A)       |
| Matrix of columns corresponding to the real and imaginary part | E.values        |                  |
| Matrix with colums corresponding to the real and imaginary part | E.vectors       |                  |
| Eigen Results as  pair of eigen values and eigen vectors (see below) | (R,V) = A.eigen | (R,V) = eigen(A) |



### Eigen Values and Eigen Vectors

Eigen values and eigen vectors are returned as (options of) an array of vectors or an array containing type spire.math.Complex. 

```scala
import com.kabouterlabs.matrix.implicits.jeigen.JeigenDenseMatrixImplicit._

    val mm1= matrix(3, 3, Array(-3.0, 1.0, -2.0, 0.0, -1.0, -1.0, 2.0, 0.0, 0.0))

    val eigr = mm1.eig
    println(eigr)
    println(eigr.vectors.map(_.mkString("\n")))
    println(eigr.values.map(_.mkString("\n")))
```



```reStructuredText
EigenResultM(Some(jeigen.DenseMatrix$EigenResult@23faf8f2))
Some(Vector((-0.5404491573562336 + -0.20309613892506811i), (-0.14361065706672058 + 0.3821552640531482i), (-0.3968385002895126 + -0.5852514029782163i))
Vector((-0.5404491573562336 + 0.20309613892506811i), (-0.14361065706672058 + -0.3821552640531482i), (-0.3968385002895126 + 0.5852514029782163i))
Vector((-0.6666666666666664 + 0.0i), (0.6666666666666671 + 0.0i), (-0.3333333333333329 + 0.0i)))
Some((-0.9999999999999996 + 1.4142135623730956i)
(-0.9999999999999996 + -1.4142135623730956i)
(-1.9999999999999998 + 0.0i))

```



## Serialization

### Synopsis



```scala
mm.csvWrite("/tmp/myfile.csv")
val mr = csvRead("/tmp/myfile.csv")
println(mm, mr)
```



```reStructuredText
({jeigen.DenseMatrix
DenseMatrix, 10 * 10:

 0.15414  0.83490  0.72279  0.46503  0.59595  0.10159  0.74101  0.77878  0.64074  0.31719 
 0.98512  0.30767  0.81694  0.13091  0.95344  0.35101  0.31062  0.17620  0.21740  0.13938 
 0.28691  0.31824  0.62313  0.93330  0.98007  0.86852  0.98573  0.25172  0.06438  0.37949 
 0.97627  0.42922  0.69139  0.59778  0.40384  0.39353  0.73436  0.47632  0.69542  0.81914 
 0.08958  0.35375  0.38292  0.65806  0.69957  0.80652  0.94376  0.34773  0.48755  0.92476 
 0.35014  0.29010  0.22483  0.20093  0.00666  0.55595  0.59384  0.11554  0.81717  0.11998 
 0.63978  0.44249  0.55701  0.55925  0.29766  0.04231  0.67029  0.24009  0.87455  0.02575 
 0.17520  0.49916  0.44057  0.81290  0.94298  0.62907  0.30640  0.17164  0.90783  0.78066 
 0.67307  0.36397  0.86295  0.21754  0.67756  0.63146  0.43146  0.00136  0.55636  0.23744 
 0.46172  0.53287  0.48871  0.51866  0.51997  0.88733  0.76277  0.84296  0.85180  0.80769 

},{jeigen.DenseMatrix
DenseMatrix, 10 * 10:

 0.15414  0.83490  0.72279  0.46503  0.59595  0.10159  0.74101  0.77878  0.64074  0.31719 
 0.98512  0.30767  0.81694  0.13091  0.95344  0.35101  0.31062  0.17620  0.21740  0.13938 
 0.28691  0.31824  0.62313  0.93330  0.98007  0.86852  0.98573  0.25172  0.06438  0.37949 
 0.97627  0.42922  0.69139  0.59778  0.40384  0.39353  0.73436  0.47632  0.69542  0.81914 
 0.08958  0.35375  0.38292  0.65806  0.69957  0.80652  0.94376  0.34773  0.48755  0.92476 
 0.35014  0.29010  0.22483  0.20093  0.00666  0.55595  0.59384  0.11554  0.81717  0.11998 
 0.63978  0.44249  0.55701  0.55925  0.29766  0.04231  0.67029  0.24009  0.87455  0.02575 
 0.17520  0.49916  0.44057  0.81290  0.94298  0.62907  0.30640  0.17164  0.90783  0.78066 
 0.67307  0.36397  0.86295  0.21754  0.67756  0.63146  0.43146  0.00136  0.55636  0.23744 
 0.46172  0.53287  0.48871  0.51866  0.51997  0.88733  0.76277  0.84296  0.85180  0.80769 

})

cat /tmp/myfile.csv
0.15413907546789152,0.8348995977694128,0.7227884567060643,0.46503450480802,0.595947615523166,0.10159288707562719,0.7410079939743957,0.7787844063474018,0.6407400744451647,0.31718950683864255
0.985123763714244,0.3076708939397962,0.8169440678434035,0.13090541405244183,0.9534403660094111,0.35101144827483033,0.31062173645831637,0.1762020037085954,0.21739938665450032,0.1393759825281825
0.2869146592538979,0.31823798893222444,0.6231292821720003,0.933295900979685,0.9800732969352105,0.8685223738762547,0.9857306826560999,0.251721497152251,0.06437821901443008,0.3794864164910342
0.9762733432197881,0.42922449229461646,0.6913866123676944,0.5977805012910483,0.403837651132517,0.39353486444321206,0.7343596459781274,0.47631874992022194,0.6954163404023956,0.8191414764462177
0.08957963546926484,0.3537538180983144,0.38292471561741026,0.6580634220746456,0.6995743531422403,0.8065173693246956,0.9437574064340373,0.3477335408452574,0.487551606741624,0.9247560302431971
0.35013947934641676,0.29010264809859043,0.22482581912653188,0.2009313865399599,0.006656412762298447,0.5559500733164903,0.5938441363664064,0.11553656637994614,0.8171690527658152,0.11997738417606374
0.639776159546066,0.4424895846895337,0.5570089965099074,0.5592520100455886,0.29766077892827747,0.042313227469349624,0.6702873633609355,0.24009308663121165,0.8745492927064217,0.025753840843653286
0.17519934115242108,0.49915953003462377,0.44056556336239494,0.812901929697022,0.9429794507923465,0.6290732760495376,0.3063952230479001,0.17164243197601858,0.9078342516215392,0.7806580478311116
0.6730729210535987,0.363972279652903,0.8629510000705228,0.2175449262553224,0.677557923524922,0.6314576718824426,0.43146434995081107,0.0013647405093277776,0.5563609235759175,0.2374406529160289
0.46171577886652426,0.5328716068832683,0.48871299884316766,0.5186631540473599,0.5199695920325623,0.8873305057795495,0.7627718340614636,0.8429647339667545,0.8517979262405598,0.8076901502966372

```

### Operations

| Operation                         | Class method   | Function       |
| --------------------------------- | -------------- | -------------- |
| Save matrix in csv format to file | A.csvWrite(fn) | csvWrite(fn,A) |
| Read matrix from csv file         |                | csvRead(fn)    |






