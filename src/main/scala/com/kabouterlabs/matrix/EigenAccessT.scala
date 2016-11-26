package com.kabouterlabs.matrix

import scala.annotation.implicitNotFound

/**
  * Created by fons on 3/31/16.
  */
@implicitNotFound("unable to get eigen values and vectors; EigenAccessT not implemented")
trait EigenAccessT[U] {
  def values():U
  def vectors():U
}
